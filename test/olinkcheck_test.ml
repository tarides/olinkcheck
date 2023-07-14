let () =
  Alcotest.run "Olinkcheck"
    [
      ( "markdown",
        [
          Alcotest.test_case "Empty text" `Quick Markdown_test.test_empty_text;
          Alcotest.test_case "Text without links" `Quick
            Markdown_test.test_text_without_links;
          Alcotest.test_case "Text with links" `Slow
            Markdown_test.test_text_with_links;
          Alcotest.test_case "Test fix links" `Quick
            Markdown_test.test_fix_links;
          Alcotest.test_case "Annotate broken links" `Quick
            Markdown_test.test_annotate;
          Alcotest.test_case "Verbose annotate broken links" `Quick
            Markdown_test.test_verbose_annotate;
        ] );
      ( "plaintext",
        [
          Alcotest.test_case "Empty text" `Quick Plaintext_test.test_empty_text;
          Alcotest.test_case "Text without links" `Quick
            Plaintext_test.test_text_without_links;
          Alcotest.test_case "Text with links" `Slow
            Plaintext_test.test_text_with_links;
          Alcotest.test_case "Test fix links" `Quick
            Plaintext_test.test_fix_links;
          Alcotest.test_case "Annotate broken links" `Quick
            Plaintext_test.test_annotate;
          Alcotest.test_case "Verbose annotate broken links" `Quick
            Plaintext_test.test_verbose_annotate;
        ] );
      ( "sexp",
        [
          Alcotest.test_case "Empty text" `Quick Sexp_test.test_empty_text;
          Alcotest.test_case "Text without links" `Quick
            Sexp_test.test_text_without_links;
          Alcotest.test_case "Text with links" `Slow
            Sexp_test.test_text_with_links;
          Alcotest.test_case "Test fix links" `Quick Sexp_test.test_fix_links;
        ] );
      ( "yaml",
        [
          Alcotest.test_case "Empty text" `Quick Yml_test.test_empty_text;
          Alcotest.test_case "Text without links" `Quick
            Yml_test.test_text_without_links;
          Alcotest.test_case "Text with links" `Slow
            Yml_test.test_text_with_links;
          Alcotest.test_case "Test fix links" `Quick Yml_test.test_fix_links;
        ] );
      ( "html",
        [
          Alcotest.test_case "Empty text" `Quick Html_test.test_empty_text;
          Alcotest.test_case "Text without links" `Quick
            Html_test.test_text_without_links;
          Alcotest.test_case "Text with links" `Slow
            Html_test.test_text_with_links;
          Alcotest.test_case "Test fix links" `Quick Html_test.test_fix_links;
        ] );
      ( "yaml_md",
        [
          Alcotest.test_case "Text with links" `Slow
            Yaml_md_test.test_text_with_links;
          Alcotest.test_case "Test fix links" `Quick Yaml_md_test.test_fix_links;
          Alcotest.test_case "Annotate broken links" `Quick
            Yaml_md_test.test_annotate;
          Alcotest.test_case "Verbose annotate broken links" `Quick
            Yaml_md_test.test_verbose_annotate;
        ] );
      ( "yaml_html",
        [
          Alcotest.test_case "Text with links" `Slow
            Yaml_html_test.test_text_with_links;
          Alcotest.test_case "Test fix links" `Quick
            Yaml_html_test.test_fix_links;
          Alcotest.test_case "Annotate broken links" `Quick
            Yaml_html_test.test_annotate;
          Alcotest.test_case "Verbose annotate broken links" `Quick
            Yaml_html_test.test_verbose_annotate;
        ] );
      ( "link-status",
        [
          Alcotest.test_case "Valid link" `Quick Link_test.test_valid_link;
          Alcotest.test_case "Invalid link" `Quick Link_test.test_invalid_link;
          Alcotest.test_case "Non-existent link" `Quick
            Link_test.test_nonexistent_link;
          Alcotest.test_case "All status codes" `Quick
            Link_test.test_all_status_codes;
        ] );
      ( "utils",
        [
          Alcotest.test_case "Exclude patterns" `Quick
            Utils_test.test_exclude_patterns;
        ] );
    ]
