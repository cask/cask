function _cask_commands() {
    local ret=1 state
    _arguments \
        ':subcommand:->subcommand' \
        '*:: :->subcmds' && ret=0

    case $state in
        subcommand)
            subcommands=(
                "path:Print Emacs exec-path (including package bin path)"
                "load-path:Print Emacs load-path (including package dependencies)"
                "help:Display this help message"
                "info:Show info about this project"
                "list:List dependencies"
                "version:Show the package version"
                "init:Create basic Cask file"
                "exec:Execute command with correct dependencies"
                "update:Update dependencies"
                "install:Install dependencies"
                "package:Create -pkg.el file"
            )
            _describe -t subcommands 'cask subcommands' subcommands && ret=0
    esac

    case "$words[1]" in
        init)
            _arguments \
                '(--dev)--dev[Run in dev mode]' && ret=0 ;;
        exec)
            _generic
            ;;
    esac

    return ret
}

compdef _cask_commands cask
