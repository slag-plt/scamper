import * as Maps from './api/maps.js'

const dict = Maps.builtinLibs

export class Search {  // GET INFO FROM DICTIONARY FOR THIS TO BE CORRECT

    tags: string[] //each element is one tag that was selected
    andOr: string //boolean searches passed only as "and" or "or" where "or" is default
    functions: string[] | undefined

    constructor(selectedTags: string[], andOr: string) {
        this.tags = selectedTags
        // For each tag in tags, get the functions from the hash map and, if they are not already
        // in functions (the array) add them there
        this.andOr = andOr
        // For each element of tags, we need to grab all elements of that key from map into temp array
        // Then put each element in temp array into functions according to boolean from andOr
        this.functions = dict.get(this.tags[0].toLowerCase())
    }
}

let userTags = Search.constructor()

for (let i = 1; i < userTags.tags.length; i++) {
    const nextFuncs = new Array(dict.get(userTags.tags[i].toLowerCase()))

    if (userTags.andOr.equals("and")) {
        var otherFuncs = new Array
        // eslint-disable-next-line @typescript-eslint/prefer-for-of
        for (let j = 0; j < nextFuncs.length; j++) {
            if (userTags.functions.include(nextFuncs[j])) {
                otherFuncs.push(nextFuncs[j])
                userTags.functions = otherFuncs
            }
        }
    } else {
        // eslint-disable-next-line @typescript-eslint/prefer-for-of
        for (let j = 0; j < nextFuncs.length; j++) {
            if (!userTags.functions.include(nextFuncs[j])) {
                userTags.functions.push(nextFuncs[j])
            }
        }
    }



    //Still need to:
    // process the list of function names stored as strings in this.functions to retrieve
    //      the appropriate documentation
    // display the gathered documentation

}