$here = Join-Path (Get-Item $MyInvocation.MyCommand.Path).Directory.Parent.FullName "bin"
$sut = (Split-Path -Leaf $MyInvocation.MyCommand.Path) -replace '\.Tests\.', '.'

. "$here\$sut"

# These tests need Pester, the popular Powershell testing framework
# From the project root, run: Invoke-Pester -EnableExit .\test\
# This will create a top-level .cask dir for the whole run and abandon it on exit
# TODO: environment modification and restoration
# TODO: Exec-Cask

Describe "Exec-Command Unit" {
    function Foo {}
    function emacs {}
    Context "With fake receivers" {
        Get-Command emacs | Select-Object -ExpandProperty CommandType | Should Be "Function"
        Mock Foo {return [psCustomObject]@{args = $args}}
        Mock emacs {return [psCustomObject]@{args = $args}}
        It "sees two args" {
            $result = cask exec Foo one two
            $result.args[0] | Should Be "one"
            $result.args[-1] | Should Be "two"
            $result.args.Count | Should Be 2
        }
        Assert-MockCalled Foo -Times 1
        It "sees two args emacs" {
            $result = cask emacs one two
            $result.args[0] | Should Be "one"
            $result.args[-1] | Should Be "two"
            $result.args.Count | Should Be 2
        }
        Assert-MockCalled emacs -Times 1
        It "sees one arg" {
            $result = cask exec foo one
            $result.args[0] | Should Be "one"
            $result.args[-1] | Should Be "one"
            $result.args[1] | Should Be $null
            $result.args.Count | Should Be 1
        }
        Assert-MockCalled Foo -Times 2
        It "sees one arg emacs" {
            $result = cask emacs one
            $result.args[0] | Should Be "one"
            $result.args[-1] | Should Be "one"
            $result.args[1] | Should Be $null
            $result.args.Count | Should Be 1
        }
        Assert-MockCalled emacs -Times 2
        It "sees no args" {
            $result = cask exec foo
            $result.args[0] | Should Be $null
            $result.args.Count | Should Be 0
        }
        Assert-MockCalled Foo -Times 3
        It "sees no args emacs" {
            $result = cask emacs
            $result.args[0] | Should Be $null
            $result.args.Count | Should Be 0
        }
        Assert-MockCalled emacs -Times 3
    }
}

Describe "Exec-Command Functional" {
    # Sanity first
    Get-Command emacs | Select-Object -ExpandProperty CommandType | Should Be "Application"
    Context "Exit status exported to invoker" {
        It "exits zero" {
            cask exec emacs -Q --batch --eval '(kill-emacs 0)'
            $? | Should Be $true
        }
        It "exits nonzero" {
            { cask exec emacs -Q --batch --eval '(kill-emacs 1)' } | Should Throw
        }
    }
    Context "Params passed to subproc" {
        It "prints args with exec" {
            $result = cask exec emacs -Q --batch --eval '(princ command-line-args)'
            $result | Should BeLike "(*\bin\emacs.exe --eval (princ command-line-args))"
        }
        It "prints args without exec" {
            $result = cask emacs -Q --batch --eval '(princ command-line-args)'
            $result | Should BeLike "(*\bin\emacs.exe --eval (princ command-line-args))"
        }
    }
}

