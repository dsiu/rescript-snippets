const fs = require('fs');

const safeParseData = data => {
    try {
        const string = data.toString()
        return string
    } catch(error) {
        return undefined
    }
}

const safeReadFile = filename =>
    new Promise(
        resolve =>
            fs.readFile(
                filename,
                (error, data) =>
                    error
                    ? resolve(undefined)
                    : resolve(safeParseData(data))
            )
    )
    .catch(
        _ =>
            Promise.resolve(undefined)
    )
module.exports = safeReadFile
