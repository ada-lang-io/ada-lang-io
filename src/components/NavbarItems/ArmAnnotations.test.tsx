import React from "react"

import { describe, expect, it, jest } from "@jest/globals"
import { fireEvent, render, screen } from "@testing-library/react"

// Import the component directly - this will test if the module resolution works
import ArmAnnotations from "./ArmAnnotations"

describe("ArmAnnotations", () => {
  it("renders without crashing", () => {
    // This test will help us see if the module can be imported properly
    expect(() => render(<ArmAnnotations />)).not.toThrow()
  })
})
