# test_that("File Managment", {
#
#   # Create test folder
#   tmp_dir <- fs::file_temp()
#   fs::dir_create(tmp_dir)
#
#   expect_message(new_project(
#     project_dir = tmp_dir, project_name = "test_project",
#     skeleton = c("documents", "data"), launch = F, rproj = F
#   ), NA)
#   expect_true(fs::dir_exists(fs::path(tmp_dir, "test_project")))
#   expect_true(fs::dir_exists(fs::path(tmp_dir, "test_project/rpgm")))
#
#   ## Create some test files inside
#   c("test.docx", "test.html") %>%
#     fs::path(tmp_dir, "test_project", "rpgm", .) %>%
#     fs::file_create()
#
#   expect_error(clean_wd(
#     input_dir = fs::path(tmp_dir, "test_project", "rpgm"),
#     output_dir = fs::path(tmp_dir, "test_project", "rpgm", "documents")
#   ), NA)
#
#   expect_true(fs::file_exists(fs::path(tmp_dir, "test_project", "rpgm", "documents")))
#   expect_true(fs::file_exists(fs::path(tmp_dir, "test_project", "rpgm", "documents", "test.docx")))
#   expect_true(fs::file_exists(fs::path(tmp_dir, "test_project", "rpgm", "documents", "test.html")))
#
#   expect_error(new_project(
#     project_dir = tmp_dir, project_name = "test_project",
#     skeleton = c("documents", "data"), launch = F, rproj = F
#   ), "already exists")
# })
