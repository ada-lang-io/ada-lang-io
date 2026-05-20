import React from "react"

import { render, screen } from "@testing-library/react"

// Import the AdmonitionWrapper directly
import AdmonitionWrapper from "./index"

// Mock the original Admonition component
jest.mock("@theme-original/Admonition", () => {
  return {
    __esModule: true,
    default: function ({
      type,
      title,
      children
    }: {
      type?: string
      title?: string
      children?: React.ReactNode
    }) {
      return React.createElement(
        "div",
        {
          "data-testid": "original-admonition",
          "data-type": type,
          "data-title": title
        },
        children
      )
    }
  }
})

// Mock the styles module
jest.mock("./styles.module.scss", () => ({
  aarm: "aarm-class"
}))

describe("AdmonitionWrapper", () => {
  afterEach(() => {
    jest.clearAllMocks()
  })

  it('renders original Admonition component when type is not "aarm"', () => {
    const props: any = {
      type: "note",
      title: "Note Title",
      children: "Note Content"
    }

    // Use React.cloneElement to avoid JSX directly in test
    const element = React.createElement(AdmonitionWrapper, props)
    render(element)

    const originalAdmonition = screen.getByTestId("original-admonition")
    expect(originalAdmonition).toBeInTheDocument()
    expect(originalAdmonition).toHaveAttribute("data-type", "note")
    expect(originalAdmonition).toHaveAttribute("data-title", "Note Title")
    expect(screen.getByText("Note Content")).toBeInTheDocument()
  })

  it('renders AARM admonition wrapper when type is "aarm"', () => {
    const props: any = {
      type: "aarm",
      aarm: "reason",
      children: "Reason Content"
    }

    render(React.createElement(AdmonitionWrapper, props))

    // Check if the div with aarm class is present
    const aarmDiv = screen.getByTestId("original-admonition").closest(".aarm-class")
    expect(aarmDiv).toBeInTheDocument()

    const originalAdmonition = screen.getByTestId("original-admonition")
    expect(originalAdmonition).toBeInTheDocument()
    expect(originalAdmonition).toHaveAttribute("data-type", "info") // reason maps to info
    expect(originalAdmonition).toHaveAttribute("data-title", "reason") // hyphens replaced with spaces
    expect(screen.getByText("Reason Content")).toBeInTheDocument()
  })

  it("uses fallback type and title when aarm type is not mapped", () => {
    const props: any = {
      type: "aarm",
      aarm: "unknown-type",
      title: "Fallback Title",
      children: "Unknown Content"
    }

    render(React.createElement(AdmonitionWrapper, props))

    const originalAdmonition = screen.getByTestId("original-admonition")
    expect(originalAdmonition).toBeInTheDocument()
    expect(originalAdmonition).toHaveAttribute("data-type", "note") // fallback type
    expect(originalAdmonition).toHaveAttribute("data-title", "Fallback Title") // uses provided title
    expect(screen.getByText("Unknown Content")).toBeInTheDocument()
  })

  it("correctly maps all AARM types to their corresponding admonition types", () => {
    const aarmMappings = [
      { aarm: "reason", expectedType: "info" },
      { aarm: "discussion", expectedType: "info" },
      { aarm: "ramification", expectedType: "note" },
      { aarm: "glossary-entry", expectedType: "tip" },
      { aarm: "proof", expectedType: "info" },
      { aarm: "correction", expectedType: "caution" },
      { aarm: "implementation-advice", expectedType: "tip" },
      { aarm: "implementation-defined", expectedType: "caution" },
      { aarm: "implementation-note", expectedType: "caution" }
    ]

    aarmMappings.forEach(({ aarm, expectedType }) => {
      const props: any = {
        type: "aarm",
        aarm,
        children: `${aarm} content`
      }

      render(React.createElement(AdmonitionWrapper, props))
      const originalAdmonition = screen.getByTestId("original-admonition")
      expect(originalAdmonition).toHaveAttribute("data-type", expectedType)
      expect(originalAdmonition).toHaveAttribute("data-title", aarm.replace("-", " "))
      expect(screen.getByText(`${aarm} content`)).toBeInTheDocument()

      // Clean up the DOM before the next iteration
      document.body.innerHTML = ""
    })
  })

  it('renders plain Admonition when type is not "aarm" regardless of other props', () => {
    const props: any = {
      type: "danger",
      title: "Danger Title",
      children: "Danger Content"
    }

    render(React.createElement(AdmonitionWrapper, props))

    const originalAdmonition = screen.getByTestId("original-admonition")
    expect(originalAdmonition).toBeInTheDocument()
    expect(originalAdmonition).toHaveAttribute("data-type", "danger")
    expect(originalAdmonition).toHaveAttribute("data-title", "Danger Title")
    expect(screen.getByText("Danger Content")).toBeInTheDocument()
  })

  it("handles children correctly", () => {
    const props: any = {
      type: "aarm",
      aarm: "tip",
      children: React.createElement("span", { "data-testid": "child-element" }, "Child Content")
    }

    render(React.createElement(AdmonitionWrapper, props))

    const childElement = screen.getByTestId("child-element")
    expect(childElement).toBeInTheDocument()
    expect(childElement).toHaveTextContent("Child Content")
  })
})
