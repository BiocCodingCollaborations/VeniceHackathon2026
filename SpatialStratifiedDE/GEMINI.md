# Gemini CLI Rules

This file contains foundational mandates for Gemini CLI within this project. These rules take absolute precedence over general defaults.

## R Coding Standards
- **Style:** Follow the [tidyverse style guide](https://style.tidyverse.org/) (e.g., use `<-` for assignment, `snake_case` for variable names).
- **Dependencies:** Avoid using `library()` or `require()` inside functions. Use the `pkg::function()` syntax for clarity and to avoid namespace conflicts.
- **Documentation:** Every new function should have roxygen2-style documentation.
- **Quarto Files:** When modifying `.qmd` files, ensure that code blocks are properly formatted and that the document remains renderable.

## Workflow & Safety
- **Verification:** Before considering a task complete, verify that R scripts can be sourced without errors.
- **Data Safety:** Never commit large datasets (e.g., `.csv`, `.rds`, `.RData`) to the repository. Ensure they are listed in `.gitignore`.
- **Commits:** Do not stage or commit changes unless explicitly requested.

## Communication
- **Brevity:** Keep explanations concise and focused on technical rationale.
- **Proactivity:** Suggest improvements to existing R code if they align with modern best practices (e.g., using `furrr` for parallel processing or `patchwork` for plots).
