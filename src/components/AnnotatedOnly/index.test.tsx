import React from "react"

import { render, screen } from "@testing-library/react"

import AnnotatedOnly from "./index"

describe("AnnotatedOnly", () => {
  it("renders children correctly", () => {
    const testContent = "Test content"

    render(
      <AnnotatedOnly>
        <span>{testContent}</span>
      </AnnotatedOnly>
    )

    expect(screen.getByText(testContent)).toBeInTheDocument()
  })

  it("wraps children in a div element", () => {
    const testContent = "Wrapped content"

    render(
      <AnnotatedOnly>
        <span>{testContent}</span>
      </AnnotatedOnly>
    )

    // Find the wrapper div that contains the child element
    const wrapperDiv = screen.getByText(testContent).closest("div")
    expect(wrapperDiv).toBeInTheDocument()
    expect(wrapperDiv?.tagName).toBe("DIV")
  })

  it("handles different types of children elements", () => {
    const testCases = [
      <div key="1">Div content</div>,
      <p key="2">Paragraph content</p>,
      <button key="3">Button text</button>,
      <span key="4">Span content</span>
    ]

    testCases.forEach((childElement) => {
      render(<AnnotatedOnly>{childElement}</AnnotatedOnly>)
      expect(
        screen.getByText(new RegExp(childElement.props.children as string))
      ).toBeInTheDocument()
    })
  })

  it("renders with proper structure", () => {
    const childContent = "Child element"

    render(
      <AnnotatedOnly>
        <div>{childContent}</div>
      </AnnotatedOnly>
    )

    const childElement = screen.getByText(childContent)
    const parentDiv = childElement.parentElement

    expect(parentDiv).toBeInTheDocument()
    expect(parentDiv?.tagName).toBe("DIV")
  })
})
