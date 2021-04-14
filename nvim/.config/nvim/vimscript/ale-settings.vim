let g:ale_linters = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['eslint'],
\   'python': ['flake8'],
\   'rust': ['cargo'],
\   'sh': ['bashate'],
\}

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'css': ['prettier'],
\   'html': ['prettier'],
\   'javascript': ['prettier'],
\   'json': ['prettier'],
\   'python': ['yapf'],
\   'rust': ['rustfmt'],
\   'sh': ['shfmt'],
\   'yaml': ['prettier'],
\}

" \   'lua': ['lua-format'],
let g:ale_fix_on_save = 1
let g:ale_rust_cargo_use_clippy = 1
