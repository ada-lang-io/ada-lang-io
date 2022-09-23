import type { ComponentTypesObject } from "@theme/NavbarItem/ComponentTypes"

import ArmAnnotations from "@site/src/components/NavbarItems/ArmAnnotations"

import ComponentTypes from "@theme-original/NavbarItem/ComponentTypes"

const ComponentTypesWrapper: ComponentTypesObject = {
  ...ComponentTypes,
  "custom-armAnnotations": ArmAnnotations
}
export default ComponentTypesWrapper
