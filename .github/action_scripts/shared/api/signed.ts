type Signed<T> = {
    proofs: {
        id: string
        signature: string
    }[]
    value: T
}

export { Signed };