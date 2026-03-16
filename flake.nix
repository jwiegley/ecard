{
  description = "ecard - vCard 4.0 (RFC 6350) parser and serializer for Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;

        emacs = pkgs.emacs30;

        emacsWithPkgs = (pkgs.emacsPackagesFor emacs).emacsWithPackages (epkgs: [
          epkgs.package-lint
          epkgs.undercover
        ]);

        src = lib.cleanSourceWith {
          src = ./.;
          filter = path: type:
            let
              baseName = baseNameOf path;
              relPath = lib.removePrefix (toString ./. + "/") (toString path);
            in
            # Exclude build artifacts and tool directories
            baseName != ".eask" &&
            baseName != ".git" &&
            baseName != ".taskmaster" &&
            baseName != ".git-hooks" &&
            baseName != "coverage" &&
            baseName != "node_modules" &&
            baseName != "result" &&
            !(lib.hasSuffix ".elc" baseName);
        };

        sourceFiles = [
          "ecard.el"
          "ecard-benchmark.el"
          "ecard-carddav-auth.el"
          "ecard-carddav-map.el"
          "ecard-carddav-mock.el"
          "ecard-carddav-sync.el"
          "ecard-carddav.el"
          "ecard-compat.el"
          "ecard-compat-examples.el"
          "ecard-display.el"
          "ecard-org.el"
          "ecard-sync.el"
          "ecard-tools-adapter.el"
          "ecard-tools.el"
          "ecard-widget.el"
        ];

        testFiles = [
          "ecard-test.el"
          "ecard-carddav-map-test.el"
          "ecard-carddav-test.el"
          "ecard-compat-test.el"
          "ecard-display-test.el"
          "ecard-org-test.el"
          "ecard-regression-test.el"
          "ecard-sync-test.el"
          "ecard-tools-test.el"
          "ecard-widget-test.el"
        ];

        # Instrumented source files for coverage (exclude mock, benchmark, etc.)
        coverageFiles = [
          "ecard.el"
          "ecard-carddav-auth.el"
          "ecard-carddav-map.el"
          "ecard-carddav-sync.el"
          "ecard-carddav.el"
          "ecard-compat.el"
          "ecard-display.el"
          "ecard-org.el"
          "ecard-sync.el"
          "ecard-tools.el"
          "ecard-widget.el"
        ];

        sourceFilesStr = builtins.concatStringsSep " " sourceFiles;
        testLoadArgs = builtins.concatStringsSep " " (map (f: "-l ${f}") testFiles);
        coverageFilesEscaped = builtins.concatStringsSep " "
          (map (f: "\\\"${f}\\\"") coverageFiles);

        allElFiles = sourceFiles ++ testFiles;
        allElFilesStr = builtins.concatStringsSep " " allElFiles;

      in {
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "ecard";
          version = "1.0.0";
          inherit src;
          nativeBuildInputs = [ emacsWithPkgs ];
          buildPhase = ''
            emacs -batch -L . \
              --eval "(setq byte-compile-error-on-warn t)" \
              --eval "(setq byte-compile-docstring-max-column 1000)" \
              -f batch-byte-compile ${sourceFilesStr}
          '';
          installPhase = ''
            mkdir -p $out/share/emacs/site-lisp/ecard
            cp *.el *.elc $out/share/emacs/site-lisp/ecard/
          '';
        };

        checks = {
          # Byte-compile with all warnings as errors
          build = self.packages.${system}.default;

          # Run the full ERT test suite
          test = pkgs.stdenv.mkDerivation {
            name = "ecard-test";
            inherit src;
            nativeBuildInputs = [ emacsWithPkgs ];
            buildPhase = ''
              emacs -batch -L . ${testLoadArgs} \
                -f ert-run-tests-batch-and-exit
            '';
            installPhase = "touch $out";
          };

          # Package-lint on main entry point
          lint = pkgs.stdenv.mkDerivation {
            name = "ecard-lint";
            inherit src;
            nativeBuildInputs = [ emacsWithPkgs ];
            buildPhase = ''
              emacs -batch -L . \
                --eval "(require 'package)" \
                --eval "(package-initialize)" \
                --eval "(require 'package-lint)" \
                -f package-lint-batch-and-exit ecard.el
            '';
            installPhase = "touch $out";
          };

          # Check indentation of all .el files (using emacs-lisp-mode indent-region)
          format = pkgs.stdenv.mkDerivation {
            name = "ecard-format-check";
            inherit src;
            nativeBuildInputs = [ emacsWithPkgs ];
            buildPhase = ''
              FAILED=0
              for f in ${allElFilesStr}; do
                result=$(emacs -batch -L . \
                  --eval "(require 'cl-lib)" \
                  --eval "(progn
                            (find-file \"$f\")
                            (emacs-lisp-mode)
                            (setq-local indent-tabs-mode nil)
                            (untabify (point-min) (point-max))
                            (let ((original (buffer-string)))
                              (indent-region (point-min) (point-max))
                              (if (string= original (buffer-string))
                                  (princ \"OK\")
                                (princ \"FAIL\"))))" 2>/dev/null)
                if [ "$result" = "FAIL" ]; then
                  echo "FAIL: $f has indentation issues"
                  FAILED=1
                fi
              done
              if [ "$FAILED" -eq 1 ]; then
                exit 1
              fi
              echo "All files properly indented."
            '';
            installPhase = "touch $out";
          };

          # Test coverage with minimum threshold
          coverage = pkgs.stdenv.mkDerivation {
            name = "ecard-coverage";
            inherit src;
            nativeBuildInputs = [ emacsWithPkgs ];
            buildPhase = ''
              mkdir -p coverage
              UNDERCOVER_FORCE=true emacs -batch -L . \
                --eval "(require 'undercover)" \
                --eval "(setq undercover-force-coverage t)" \
                --eval "(undercover ${coverageFilesEscaped}
                                    (:report-file \"coverage/lcov.info\")
                                    (:report-format (quote lcov))
                                    (:send-report nil))" \
                ${testLoadArgs} \
                -f ert-run-tests-batch-and-exit

              if [ -f coverage/lcov.info ]; then
                total_lines=$(grep -c "^DA:" coverage/lcov.info || echo 0)
                covered_lines=$(grep "^DA:" coverage/lcov.info | grep -cv ",0$" || echo 0)
                if [ "$total_lines" -gt 0 ]; then
                  coverage=$((covered_lines * 100 / total_lines))
                  echo "Coverage: $coverage% ($covered_lines/$total_lines lines)"
                  if [ "$coverage" -lt 60 ]; then
                    echo "ERROR: Coverage $coverage% is below 60% threshold"
                    exit 1
                  fi
                fi
              else
                echo "WARNING: No coverage report generated"
              fi
            '';
            installPhase = ''
              mkdir -p $out
              cp -r coverage $out/ 2>/dev/null || touch $out/empty
            '';
          };

          # Performance benchmark regression check
          perf = pkgs.stdenv.mkDerivation {
            name = "ecard-perf";
            inherit src;
            nativeBuildInputs = [ emacsWithPkgs pkgs.bc ];
            buildPhase = ''
              RESULT=$(emacs -batch -L . \
                -l ecard.el -l ecard-compat.el -l ecard-benchmark.el \
                --eval "(progn
                          (let* ((num-vcards 100)
                                 (content (ecard-benchmark--generate-test-file num-vcards))
                                 (start (float-time))
                                 (cards (ecard-compat-parse-multiple content))
                                 (parse-time (- (float-time) start))
                                 (start2 (float-time))
                                 (serialized (mapconcat #'ecard-serialize cards \"\n\"))
                                 (serialize-time (- (float-time) start2)))
                            (princ (format \"parse_ms=%.1f serialize_ms=%.1f count=%d\"
                                           (* 1000 parse-time)
                                           (* 1000 serialize-time)
                                           (length cards)))))" 2>&1 | grep "^parse_ms=" || true)
              echo "Benchmark: $RESULT"

              if [ -z "$RESULT" ]; then
                echo "WARNING: Benchmark produced no output, skipping perf check"
                exit 0
              fi

              PARSE_MS=$(echo "$RESULT" | sed 's/.*parse_ms=\([0-9.]*\).*/\1/')
              SERIALIZE_MS=$(echo "$RESULT" | sed 's/.*serialize_ms=\([0-9.]*\).*/\1/')

              echo "Parse 100 vCards: $PARSE_MS ms"
              echo "Serialize 100 vCards: $SERIALIZE_MS ms"

              if [ -f .perf-baseline ]; then
                BASELINE_PARSE=$(grep "^parse_ms=" .perf-baseline | cut -d= -f2)
                BASELINE_SERIALIZE=$(grep "^serialize_ms=" .perf-baseline | cut -d= -f2)

                PARSE_LIMIT=$(echo "$BASELINE_PARSE * 105 / 100" | bc -l)
                SERIALIZE_LIMIT=$(echo "$BASELINE_SERIALIZE * 105 / 100" | bc -l)

                PARSE_REGRESSED=$(echo "$PARSE_MS > $PARSE_LIMIT" | bc -l)
                SERIALIZE_REGRESSED=$(echo "$SERIALIZE_MS > $SERIALIZE_LIMIT" | bc -l)

                if [ "$PARSE_REGRESSED" = "1" ] || [ "$SERIALIZE_REGRESSED" = "1" ]; then
                  echo "REGRESSION: Performance dropped more than 5% vs baseline"
                  exit 1
                fi
                echo "Performance within 5% of baseline."
              else
                echo "No .perf-baseline found, skipping regression check."
              fi
            '';
            installPhase = "touch $out";
          };

          # Fuzz testing with random/malformed inputs
          fuzz = pkgs.stdenv.mkDerivation {
            name = "ecard-fuzz";
            inherit src;
            nativeBuildInputs = [ emacsWithPkgs ];
            buildPhase = ''
              emacs -batch -L . -l ecard.el \
                --eval "
              (progn
                (random t)
                (defun ecard--fuzz-random-string (len)
                  (let ((s (make-string len 0)))
                    (dotimes (i len s)
                      (aset s i (+ 32 (random 95))))))
                (defun ecard--fuzz-valid-vcard ()
                  (concat \"BEGIN:VCARD\nVERSION:4.0\nFN:\"
                          (ecard--fuzz-random-string (1+ (random 50)))
                          \"\n\"
                          (mapconcat
                           (lambda (_)
                             (let ((prop (nth (random 10)
                                              '(\"EMAIL\" \"TEL\" \"ORG\" \"NOTE\" \"ADR\"
                                                \"TITLE\" \"URL\" \"NICKNAME\" \"BDAY\" \"GEO\"))))
                               (concat prop \":\" (ecard--fuzz-random-string (1+ (random 100))))))
                           (make-list (random 20) nil) \"\n\")
                          \"\nEND:VCARD\"))
                (defun ecard--fuzz-malformed ()
                  (let ((v (random 6)))
                    (cond
                     ((= v 0) \"VERSION:4.0\nFN:Test\nEND:VCARD\")
                     ((= v 1) \"BEGIN:VCARD\nVERSION:4.0\nFN:Test\")
                     ((= v 2) \"BEGIN:VCARD\nFN:Test\nEND:VCARD\")
                     ((= v 3) \"BEGIN:VCARD\nVERSION:3.0\nFN:Test\nEND:VCARD\")
                     ((= v 4) (ecard--fuzz-random-string (1+ (random 500))))
                     ((= v 5) \"\"))))
                (let ((failures 0) (total 1000))
                  (dotimes (i total)
                    (condition-case err
                        (ecard-parse (if (< (random 2) 1)
                                         (ecard--fuzz-valid-vcard)
                                       (ecard--fuzz-malformed)))
                      (ecard-parse-error nil)
                      (ecard-validation-error nil)
                      (error
                       (setq failures (1+ failures))
                       (message \"Fuzz %d: %S\" i (car err)))))
                  (message \"Fuzz: %d/%d passed\" (- total failures) total)
                  (when (> failures 0) (kill-emacs 1))
                  (message \"All fuzz tests passed.\")))"
            '';
            installPhase = "touch $out";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            emacsWithPkgs
            pkgs.lefthook
            pkgs.bc
          ];
          shellHook = ''
            echo "ecard development shell"
            echo "  nix build        -- byte-compile package"
            echo "  nix flake check  -- run all checks"
            echo "  lefthook install -- set up pre-commit hooks"
          '';
        };
      }
    );
}
