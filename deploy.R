usethis::use_github_action(
  url = "https://github.com/posit-dev/r-shinylive/blob/actions-v1/examples/deploy-app.yaml"
)

library(webr)

shinylive::export(appdir = here::here(), here::here("site"))

httpuv::runStaticServer(
  port = 8000,
  here::here("site"))
