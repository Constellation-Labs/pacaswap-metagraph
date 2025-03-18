import { z } from "zod";

const lastRefSchema = z.object({
    ordinal: z.number(),
    hash: z.string()
})

type LastRef = z.infer<typeof lastRefSchema>

export { lastRefSchema, LastRef }