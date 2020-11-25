load_arg.dbiconf_loader_keyring <- function(loader) {
  assert_package("keyring")
  load_arg_default(loader, keyring::key_get)
}
