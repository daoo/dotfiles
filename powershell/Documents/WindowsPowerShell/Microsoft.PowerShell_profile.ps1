Set-PSReadlineOption -EditMode vi

# Remove PowerShell aliases that conflicts with programs
Remove-Item alias:ls
Remove-Item alias:curl
Remove-Item alias:cat
Remove-Item alias:rm
Remove-Item alias:r

# C:\WINDOWS\system32\find.exe overrides scoop shim,
# fix by explicitly aliasing.
Set-Alias find find.ps1

function ls { ls.exe --human-readable --group-directories-first --color=yes $args }
Set-Alias l ls
function ll { ls -l $args }
function la { ls --almost-all $args }
function lla { ls -l --almost-all $args }

Set-Alias g git

Set-PSReadlineKeyHandler -Chord Ctrl+P -Function PreviousHistory
Set-PSReadlineKeyHandler -Chord Ctrl+N -Function NextHistory
Set-PSReadlineKeyHandler -Chord Ctrl+R -Function ReverseSearchHistory
Set-PSReadlineKeyHandler -Key Tab -Function Complete

function make-link ($target, $link) {
    New-Item -Path $link -ItemType SymbolicLink -Value $target
}

function Prompt
{
    $width = (get-host).UI.RawUI.MaxWindowSize.Width
    $dir = (get-location).Path.Replace($HOME, '~')
    $hostinfo = $ENV:USERNAME + "@" + $ENV:COMPUTERNAME
    $timedate = (get-date -uformat "%Y-%m-%d %H:%M:%S")

    $left = "-(" + $dir + ")-"
    $right = "-(" + $hostinfo + ")-[" + $timedate + "]-"
    $fill = "-" * ($width - $left.length - $right.length)
    Write-Host "-(" -NoNewline -ForegroundColor White
    Write-Host $dir -NoNewline -ForegroundColor Magenta
    Write-Host ")-" -NoNewline -ForegroundColor White
    Write-Host $fill -NoNewline -ForegroundColor White
    Write-Host '-(' -NoNewline -ForegroundColor White
    Write-Host $hostinfo -NoNewline -ForegroundColor DarkBlue
    Write-Host ')-[' -NoNewline -ForegroundColor White
    Write-Host $timedate -NoNewline -ForegroundColor DarkBlue
    Write-Host ']-' -NoNewline -ForegroundColor White
    Write-Host "-->" -NoNewline -ForegroundColor White
    return " "
}
