Get-ChildItem -Path $env:USERPROFILE -Filter ".*" -Force | foreach {$_.Attributes = "Hidden"}
Get-ChildItem -Path $env:USERPROFILE -Filter "_*" -Force | foreach {$_.Attributes = "Hidden"}
