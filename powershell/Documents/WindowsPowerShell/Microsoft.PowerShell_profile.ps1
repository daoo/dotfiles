Import-Module PSReadLine

Set-PSReadlineOption -BellStyle None
Set-PSReadlineOption -EditMode vi

# Remove PowerShell aliases that conflicts with programs
Remove-Alias ls

function ls { & $ENV:USERPROFILE\scoop\apps\git-with-openssh\current\usr\bin\ls.exe --human-readable --group-directories-first --color=yes $args }
Set-Alias l ls
function ll { ls -l $args }
function la { ls --almost-all $args }
function lla { ls -l --almost-all $args }

Set-Alias g git.exe

Set-PSReadlineKeyHandler -Chord Ctrl+P -Function PreviousHistory
Set-PSReadlineKeyHandler -Chord Ctrl+N -Function NextHistory
Set-PSReadlineKeyHandler -Chord Ctrl+R -Function ReverseSearchHistory
Set-PSReadlineKeyHandler -Key Tab -Function Complete

function Prompt
{
    $width = (get-host).UI.RawUI.MaxWindowSize.Width
    $dir = (get-location).Path.Replace($HOME, '~')
    $hostinfo = $ENV:USERNAME + "@" + $ENV:COMPUTERNAME
    $timedate = (get-date -uformat "%Y-%m-%d %H:%M:%S")

    $Host.UI.RawUI.WindowTitle = $dir

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
