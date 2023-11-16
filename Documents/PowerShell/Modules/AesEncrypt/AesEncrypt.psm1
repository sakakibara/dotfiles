function New-AesManagedObject
{
  param
  (
    $Key,
    $IV,
    [string]$Mode = "CBC"
  )

  $aesManaged = New-Object -TypeName System.Security.Cryptography.AesManaged

  if ($Mode="CBC")
  { $aesManaged.Mode = [System.Security.Cryptography.CipherMode]::CBC
  } elseif ($Mode="CFB")
  {$aesManaged.Mode = [System.Security.Cryptography.CipherMode]::CFB
  } elseif ($Mode="CTS")
  {$aesManaged.Mode = [System.Security.Cryptography.CipherMode]::CTS
  } elseif ($Mode="ECB")
  {$aesManaged.Mode = [System.Security.Cryptography.CipherMode]::ECB
  } elseif ($Mode="OFB")
  {$aesManaged.Mode = [System.Security.Cryptography.CipherMode]::OFB
  }

  $aesManaged.Padding = [System.Security.Cryptography.PaddingMode]::PKCS7
  $aesManaged.BlockSize = 128
  $aesManaged.KeySize = 256

  if ($IV)
  {
    if ($IV.getType().Name -eq "String")
    {
      $aesManaged.IV = [System.Convert]::FromBase64String($IV)
    } else
    {
      $aesManaged.IV = $IV
    }
  }
  if ($Key)
  {
    if ($Key.getType().Name -eq "String")
    {
      $aesManaged.Key = [System.Convert]::FromBase64String($Key)
    } else
    {
      $aesManaged.Key = $Key
    }
  }
  $aesManaged
}

function New-AesKey
{
  $aesManaged = New-AesManagedObject
  $aesManaged.GenerateKey()
  [System.Convert]::ToBase64String($aesManaged.Key)
}

function Get-EncryptString
{
  param
  (
    $Key,
    $PlainText
  )

  $bytes = [System.Text.Encoding]::UTF8.GetBytes($PlainText)
  $aesManaged = New-AesManagedObject $Key
  $encryptor = $aesManaged.CreateEncryptor()
  $encryptedData = $encryptor.TransformFinalBlock($bytes, 0, $bytes.Length);
  [byte[]] $fullData = $aesManaged.IV + $encryptedData
  [System.Convert]::ToBase64String($fullData)
}

function Get-DecryptString
{
  param
  (
    $Key,
    $EncryptedStringWithIV
  )

  $bytes = [System.Convert]::FromBase64String($EncryptedStringWithIV)
  $IV = $bytes[0..15]
  $aesManaged = New-AesManagedObject $Key $IV
  $decryptor = $aesManaged.CreateDecryptor();
  $unencryptedData = $decryptor.TransformFinalBlock($bytes, 16, $bytes.Length - 16);
  $aesManaged.Dispose()
  [System.Text.Encoding]::UTF8.GetString($unencryptedData).Trim([char]0)
}

function Write-HostEncryptString
{
  param
  (
    $PlainText
  )

  $key = New-AesKey

  "== Powershell AES CBC Encyption=="
  "`nKey: "+$key

  $encryptedString = Get-EncryptString $key $PlainText

  $plain = Get-DecryptString $key $encryptedString

  $bytes = [System.Convert]::FromBase64String($encryptedString)

  $IV = $bytes[0..15]
  "Salt: " +  [System.Convert]::ToHexString($IV)
  "Salt: " +  [System.Convert]::ToBase64String($IV)

  "`nEncrypted: "+$encryptedString

  "Decrypted: "+$plain
}

function Write-HostDecryptString
{
  param
  (
    $Key,
    $EncryptedString
  )

  "== Powershell AES CBC Encyption=="
  "`nKey: "+$Key

  $plain = Get-DecryptString $key $EncryptedString

  $bytes = [System.Convert]::FromBase64String($EncryptedString)

  $IV = $bytes[0..15]
  "Salt: " +  [System.Convert]::ToHexString($IV)
  "Salt: " +  [System.Convert]::ToBase64String($IV)

  "`nEncrypted: "+$EncryptedString

  "Decrypted: "+$plain
}
