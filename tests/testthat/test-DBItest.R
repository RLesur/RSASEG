DBItest::make_context(
  drv = SASEG("C:\\Program Files\\SAS94M2\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll"), 
  connect_args = list(
    DLLFilePath = "C:\\Program Files\\SAS94M2\\SASEnterpriseGuide\\7.1\\SASEGScripting.dll",
    profile = rstudioapi::askForPassword("Profile Name:"),
    server = rstudioapi::askForPassword("Server Name:")
  ),
  set_as_default = TRUE,
  tweaks = DBItest::tweaks(constructor_relax_args = TRUE, omit_blob_tests = TRUE),
  name = "Romain Lesur's profile"
)
DBItest::test_getting_started()
DBItest::test_driver()
DBItest::test_connection()