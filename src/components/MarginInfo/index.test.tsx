import React from "react"

import { render, screen } from "@testing-library/react"

import MarginInfo from "./index"

// Mock the InfoHover component since it's external to MarginInfo logic
jest.mock("@site/src/components/InfoHover", () => ({
  __esModule: true,
  default: ({ text }: { text: (string | JSX.Element)[] }) => (
    <div data-testid="info-hover">
      {Array.isArray(text) ? text.map((item, index) => <div key={index}>{item}</div>) : text}
    </div>
  )
}))

describe("MarginInfo", () => {
  it("renders with AI12 items correctly", () => {
    const items = ["AI12-001", "AI12-002"]

    render(<MarginInfo items={items} />)

    // Check that the links are created with the correct URLs
    expect(screen.getByText("AI12-001")).toHaveAttribute(
      "href",
      "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-001.TXT"
    )
    expect(screen.getByText("AI12-002")).toHaveAttribute(
      "href",
      "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-002.TXT"
    )
  })

  it("renders with AI05 items correctly", () => {
    const items = ["AI05-001", "AI05-002"]

    render(<MarginInfo items={items} />)

    // Check that the links are created with the correct URLs
    expect(screen.getByText("AI05-001")).toHaveAttribute(
      "href",
      "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI05s/AI05-001.TXT"
    )
    expect(screen.getByText("AI05-002")).toHaveAttribute(
      "href",
      "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI05s/AI05-002.TXT"
    )
  })

  it("renders with AI95 items correctly", () => {
    const items = ["AI95-001-01", "AI95-002-02"]

    render(<MarginInfo items={items} />)

    // Check that the links are created with the correct URLs
    // AI95-001-01 should extract 001-0 from positions 5-10 (substring 5, 10) and create link to AI-001-0.TXT
    expect(screen.getByText("AI95-001-01")).toHaveAttribute(
      "href",
      "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AIs/AI-001-0.TXT"
    )
    expect(screen.getByText("AI95-002-02")).toHaveAttribute(
      "href",
      "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AIs/AI-002-0.TXT"
    )
  })

  it("renders non-AI items as spans", () => {
    const items = ["RegularText", "Another Item"]

    render(<MarginInfo items={items} />)

    // Check that non-AI items are rendered as spans
    expect(screen.getByText("RegularText")).toBeInTheDocument()
    expect(screen.getByText("Another Item")).toBeInTheDocument()

    // Verify they are span elements
    const regularTextElement = screen.getByText("RegularText").closest("span")
    expect(regularTextElement).toBeInTheDocument()

    const anotherItemElement = screen.getByText("Another Item").closest("span")
    expect(anotherItemElement).toBeInTheDocument()
  })

  it("handles mixed items correctly", () => {
    const items = ["AI12-001", "RegularText", "AI05-002", "AI95-003-03"]

    render(<MarginInfo items={items} />)

    // Check AI12 item
    expect(screen.getByText("AI12-001")).toHaveAttribute(
      "href",
      "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI12s/AI12-001.TXT"
    )

    // Check regular text item
    expect(screen.getByText("RegularText")).toBeInTheDocument()
    expect(screen.getByText("RegularText").closest("span")).toBeInTheDocument()

    // Check AI05 item
    expect(screen.getByText("AI05-002")).toHaveAttribute(
      "href",
      "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AI05s/AI05-002.TXT"
    )

    // Check AI95 item
    expect(screen.getByText("AI95-003-03")).toHaveAttribute(
      "href",
      "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/AIs/AI-003-0.TXT"
    )
  })

  it("renders empty items array without errors", () => {
    const items: string[] = []

    render(<MarginInfo items={items} />)

    // Component should render without throwing errors
    expect(screen.queryByTestId("info-hover")).toBeInTheDocument()
  })
})
