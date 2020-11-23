#' @export
loader_resolve.dbiconf_loader_keyring <- function(loader) {
  assert_package("keyring")
  loader_resolve_default(loader, keyring::key_get)
}
