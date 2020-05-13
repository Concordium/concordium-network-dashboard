function allItems() {
  var items = [],
      keys = Object.keys(localStorage)

  for (var i=0, key; key = keys[i]; i++) {
    if (key.split('-').length == 2) {
      items.push(getItem(key))
    }
  }
  return items;
}

function getItem(key) {
  var data = window.localStorage.getItem(key)
  var [id,type] = key.split('-')
  if (data === null) {
    throw "Error: localStorage document with key " + key + " not found.";
  }
  return { id: id, type: type, value: JSON.parse(data) }
}

export function setup (app) {
  app.ports.saveDoc.subscribe(function(item) {
    try {
      // console.log("Saving item: ", item)
	    window.localStorage.setItem(item.id + "-" + item.type, JSON.stringify(item.value))
      // Immediately load persisted/changed docs so Elm is synced
      app.ports.receiveDoc.send(item)
    } catch (e) {
	    // Local storage is full or blocked.
	    console.error(e)
    }
  })

  // app.ports.fetchDoc.subscribe(function(key) {
  // 	try {
	//     app.ports.receiveDoc.send(getItem(key))
  // 	} catch (e) {
	//     // Local storage is blocked.
	//     // @TODO: do not send null
	//     // app.ports.receiveDoc.send(null)
	//     console.error(e)
  // 	}
  // })

  app.ports.loadAll.subscribe(function(){
    try {
      var items = allItems()

      for (var i=0, item; item = items[i]; i++) {
        app.ports.receiveDoc.send(item)
      }

    } catch (e) {
	    // Local storage is blocked.
	    console.error(e)
    }
  })

  // app.ports.clear.subscribe(function(){
  //   localStorage.clear()
  // })
}
