// Generated by ReScript, PLEASE EDIT WITH CARE


const dataA = {
  body: "My hello content",
  attributes: {
    id: 1,
    title: "Hello",
    date: new Date("2020-12-14 19:47:57")
  }
}

const dataB = {
  body: "My world content",
  attributes: {
    title: "World",
    template: "main"
  }
}
;

function handleDataA(data) {
  let body = data.body;
  let id = data.attributes.id;
  let title = data.attributes.title;
  let date = data.attributes.date;
  console.log(body);
  console.log(id);
  console.log(title);
  console.log(date);
}

function handleDataB(data) {
  let body = data.body;
  let title = data.attributes.title;
  let template = data.attributes.template;
  console.log(body);
  console.log(title);
  console.log(template);
}

handleDataA(dataA);

handleDataB(dataB);

export {
  handleDataA,
  handleDataB,
}
/*  Not a pure module */
