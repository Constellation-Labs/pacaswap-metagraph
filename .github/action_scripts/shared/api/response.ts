import { z } from "zod";

const singleResponseSchema = <T>(schema: z.ZodSchema<T>) => z.object({
    data: schema
})

type SingleResponse<T> = z.infer<ReturnType<typeof singleResponseSchema<T>>>

export { singleResponseSchema, SingleResponse }