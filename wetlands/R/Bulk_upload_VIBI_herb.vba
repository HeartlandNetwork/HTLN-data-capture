Private Sub Load_VIBI_herb_Click()

  Dim LResponse As Integer
  Dim SQLmakecopy, SQLinsertdata, SQLdeletedata As String
  
  SQLmakecopy = "SELECT VIBI_Herb_ID, EventID, LocationID, Species, VoucherNo, " & _
    "ModNo, CovCode INTO tbl_VIBI_Herb_bak FROM tbl_VIBI_Herb"
    
  SQLinsertdata = "INSERT INTO tbl_VIBI_Herb ( EventID, LocationID, Species, ModNo, CovCode ) " & _
    "SELECT tbl_Load_VIBI_herb.EventID, tbl_Load_VIBI_herb.LocationID, " & _
    " tbl_Load_VIBI_herb.Species, tbl_Load_VIBI_herb.Module, tbl_Load_VIBI_herb.CoverClass " & _
    " FROM tbl_Load_VIBI_herb;"
    
   SQLdeletedata = "DELETE tbl_Load_VIBI_herb.EventID, tbl_Load_VIBI_herb.FeatureID, " & _
     " tbl_Load_VIBI_herb.LocationID, tbl_Load_VIBI_herb.Species, tbl_Load_VIBI_herb.Module, " & _
     " tbl_Load_VIBI_herb.CoverClass, tbl_Load_VIBI_herb.CoverClassAll, " & _
     " tbl_Load_VIBI_herb.EditDate, tbl_Load_VIBI_herb.Timestamp " & _
     " FROM tbl_Load_VIBI_herb;"

  
  LResponse = MsgBox("WARNING - This procedure modifies data in tbl_VIBI_Herb. Ensure backup copy of original", vbYesNo)
  If LResponse = vbYes Then
    'Continue
  Else
    Exit Sub
  End If

  DoCmd.RunSQL SQLmakecopy
  
  ' Need to delete records in tbl_Load_VIBI_herb
  
  DoCmd.RunSQL SQLdeletedata

  DoCmd.TransferSpreadsheet acImport, , "tbl_Load_VIBI_herb", "C:\Users\GRowell\work\Databases\Wetlands\20230802-Survey123\Load_VIBI_herb.xlsx", True

  DoCmd.RunSQL SQLinsertdata

End Sub