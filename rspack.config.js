import { LynxEncodePlugin, LynxTemplatePlugin } from "@lynx-js/template-webpack-plugin";
import { defineConfig } from "@rspack/cli";

export default defineConfig({
  entry: {
    main: './dist/all.js',
  },
  module: {
	rules: [
      {
		test: /\.js$/,
		use: [{
          loader: "builtin:swc-loader",
		  options: {
		    jsc: {
				parser: {
				    syntax: "ecmascript"
  		 		}
		    }
		  }
        }]
      }
	]
  },
  plugins: [
    new LynxEncodePlugin(),
    new LynxTemplatePlugin({
      filename: "main.lynx.bundle",
      intermediate: "main",
    }),
    /**
     * @param {import("@rspack/core").Compiler} compiler
     */
    (compiler) => {
      compiler.hooks.thisCompilation.tap(
        "MarkMainThreadWebpackPlugin",
        /**
         * @param {import("@rspack/core").Compilation} compilation
         */
        (compilation) => {
          compilation.hooks.processAssets.tap(
            "MarkMainThreadWebpackPlugin",
            () => {
              const asset = compilation.getAsset(`main.js`);
              compilation.updateAsset(asset.name, asset.source, {
                ...asset.info,
                "lynx:main-thread": true,
              });
            },
          );
        },
      );
    },
  ]
});
