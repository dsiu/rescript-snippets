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
  var body = data.body;
  var id = data.attributes.id;
  var title = data.attributes.title;
  var date = data.attributes.date;
  console.log(body);
  console.log(id);
  console.log(title);
  console.log(date);
  
}

function handleDataB(data) {
  var body = data.body;
  var title = data.attributes.title;
  var template = data.attributes.template;
  console.log(body);
  console.log(title);
  console.log(template);
  
}

handleDataA(dataA);

handleDataB(dataB);

export {
  handleDataA ,
  handleDataB ,
  
}
/*  Not a pure module */