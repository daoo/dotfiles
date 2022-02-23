function DiffFile($file1, $file2)
{
    if (!($str1 = $(Get-Content $file1 -ErrorAction SilentlyContinue)))
    {
        Write-Host $file1 does not exist
        return
    }
    if (!($str2 = $(Get-Content $file2 -ErrorAction SilentlyContinue)))
    {
        Write-Host $file2 copied
        Copy-Item -Path $file1 -Destination $file2
        return
    }
    if (Compare-Object -ReferenceObject $str1 -DifferenceObject $str2)
    {
        Write-Host $file2 differs
        & nvim.exe -d $file1 $file2
        return
    }
    Write-Host $file2 up to date
}

DiffFile "git\.config\git\config" "$HOME\.gitconfig"
DiffFile "nvim\.config\nvim\ftplugin\cpp.vim" "$HOME\AppData\Local\nvim\ftplugin\cpp.vim"
DiffFile "nvim\.config\nvim\ftplugin\git.vim" "$HOME\AppData\Local\nvim\ftplugin\git.vim"
DiffFile "nvim\.config\nvim\init.vim" "$HOME\AppData\Local\nvim\init.vim"
DiffFile "powershell\Documents\PowerShell\Microsoft.PowerShell_profile.ps1" "$HOME\Documents\PowerShell\Microsoft.PowerShell_profile.ps1"
DiffFile "tig\.config\tig\config" "$HOME\.tigrc"
