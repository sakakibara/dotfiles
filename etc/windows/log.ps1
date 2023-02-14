# Log recording script
# It can be used to record the time of logon or logoff by setting it as
# logon/logoff script via group policy editor (gpedit.msc)
#
# Example parameters:
# $logPath = C:\log\logon.txt
# $recordType = LOGON

Param(
    [Parameter(Position=0,
               HelpMessage="Path to save the log file",
               Mandatory=$true)]
    [String]$logPath,
    [Parameter(Position=1,
               HelpMessage="Log record type",
               Mandatory=$true)]
    [String]$recordType
)

# Right pad record type name
$recordType = $recordType.PadRight(6)

# Getting dates
$currentDateTime = Get-Date
$longDateTime = $currentDateTime.ToString('yyyy-MM-dd HH:mm:ss')

# Record to log
$record = "$($longDateTime) $($recordType) :: $($env:UserName) - $($env:ComputerName)"

# Append the record to the log
$record | Out-File $logPath -Encoding default -Append
