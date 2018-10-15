function getchar() {
    const stdin = document.getElementById("stdin");
    var inputBuffer = stdin.textContent;
    if (inputBuffer.length == 0) {
        inputBuffer = prompt("More input for stdin plz:");
    }

    const output = inputBuffer.charCodeAt(0);
    stdin.textContent = inputBuffer.substr(1);
    return output;
}

function putchar(charCode) {
    console.log(charCode)
    const stdout = document.getElementById("stdout");
    stdout.textContent = stdout.textContent + String.fromCharCode(charCode);
}

function main(wasm) {
    wasm.exports.bf();
}

fetch("main.wasm").then(reponse =>
    reponse.arrayBuffer()
).then(bytes =>
    WebAssembly.instantiate(bytes, {
        "bf": {
            putchar: putchar,
            getchar: getchar
        }
    })
).then(result =>
    result.instance
).then(main);
