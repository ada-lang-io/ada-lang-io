import React from "react"

import { FaInfo as InfoIcon } from "react-icons/fa"

import { Badge, HoverCard, Stack, Text } from "@mantine/core"

import classes from "./index.module.scss"

interface InfoHoverProps {
  readonly text: (string | JSX.Element)[]
  readonly width: number
}

export default function InfoHover({ text, width = 300 }: InfoHoverProps): JSX.Element {
  return (
    <HoverCard width={width} shadow="md">
      <HoverCard.Target>
        <Badge size="xs" radius="md" variant="outline" className={classes.infoBadge}>
          <InfoIcon />
        </Badge>
      </HoverCard.Target>
      <HoverCard.Dropdown>
        <Text size="sm">
          <Stack spacing={8}>
            {text.map((line, i) => (
              <div key={i}>{line}</div>
            ))}
          </Stack>
        </Text>
      </HoverCard.Dropdown>
    </HoverCard>
  )
}
