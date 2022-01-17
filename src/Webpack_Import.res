//
// Importing files using Webpack
// ref:
// https://kevanstannard.github.io/rescript-blog/webpack-import-file.html

module Webpack = {
  type url = {href: string, pathname: string}

  @scope(("import", "meta")) @val external importMetaUrl: string = "url"
  @new external makeFileUrl: (string, string) => url = "URL"
}

let url = Webpack.makeFileUrl("./images/logo.png", Webpack.importMetaUrl)

// Which produces a URL that can be used in front end code.
// var url = new URL("./images/logo.png", import.meta.url);
