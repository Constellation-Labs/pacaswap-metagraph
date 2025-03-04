import axios from "axios";

export const getCalculatedState = async (
    ammL0Url: string,
) => {
    const { data } = await axios.get(`${ammL0Url}/v1/calculated-state/latest`);
    return data.calculatedState;
}  
