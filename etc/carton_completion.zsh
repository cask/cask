function _carton_commands() {
    local ret=1 state
    _arguments \
        ':subcommand:->subcommand' \
        '*:: :->subcmds' && ret=0

    case $state in
        subcommand)
            subcommands=(
                "package:Create -pkg.el file"
                "install:Install dependencies"
                "update:Update dependencies"
                "exec:Execute command with correct dependencies"
                "init:Create basic Carton file"
                "version:Show package version"
                "list:List dependencies"
                "info:Show info about this project"
                "help:Display help message"
            )
            _describe -t subcommands 'carton subcommands' subcommands && ret=0
    esac

    case "$words[1]" in
        init)
            _arguments \
                '(--dev)--dev[Use if project is for package development]' && ret=0 ;;
        exec)
            _generic
            ;;
    esac

    return ret
}

compdef _carton_commands carton
