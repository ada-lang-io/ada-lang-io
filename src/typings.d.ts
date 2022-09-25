declare module "*.module.scss" {
  const classes: { readonly [key: string]: string }
  export default classes
}

declare module "*.ads" {
  const content: string
  export default content
}

declare module "*.adb" {
  const content: string
  export default content
}

type PluginDataAlireVersion = {
  readonly alireVersion: string
}
