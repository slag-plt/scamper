import * as L from "./lang"
import * as U from "./util"
import '../../public/css/styles.css'

function vectorHeight(vec: L.Vector, index = 0): number {
    let height = 1;
    for(let i = index; i < vec.length; i++) {
      const e = vec[i]
      if(typeof e === 'string' || typeof e === 'number' || typeof e === 'boolean') {
        height = height + 1
      } 
      else if(U.isList(e)) {
        height = height + listHeight(e) + 1
      } else if(U.isPair(e)) {
        height = height + pairHeight(e)
      } else if(U.isArray(e)) {
        height = height + vectorHeight(e, 0)
      } else if (U.isStruct(e)) {
        height = height + structHeight(e) + 2
      }
    }
    return height + 3
  }
  
  export function drawVectorHTML(vector: L.Vector, ancestor = "0", imgID: number = Math.random()): HTMLDivElement {
    //Container for html elements
    const div = document.createElement('div');
    div.ariaLabel = 'object type vector';
    div.tabIndex = 0;
    div.style.position = 'relative';
  
    //loops through the vector, making the visualization pieces for each element
    vector.forEach((e: L.Value, i: number) => {
      //container for all the html elements for one vector element
      const col = document.createElement('div');
      col.className = 'vector-style';
      if (i > 0){
        col.style.marginLeft = '-3px'
        col.style.position = 'absolute';
      }
      col.style.left = `${30 * i}px`
  
      //creates the box elements of the vector
      const box = document.createElement('div');
      const index = document.createElement('div');
      const indexVal = i;
      index.className = 'index-box';
      index.textContent = indexVal + '';
      index.setAttribute('aria-hidden', 'true');
      col.appendChild(index);
      box.className = 'vector-box';
      box.tabIndex = 0;
      box.id = `${imgID}:` + ancestor + `:` + i + ` val`
      box.addEventListener('keydown', (e) => {
        keyHandler(e.key, box, 'vector');
      })
      if(U.isList(e)) {
        box.ariaDescription = `vector index ${indexVal} contains a list`
        box.ariaLabel = `vector index ${indexVal} contains a list`
      }if(U.isPair(e)) {
        box.ariaDescription = `vector index ${indexVal} contains a pair`
        box.ariaLabel = `vector index ${indexVal} contains a pair`
      } else if(U.isArray(e)) {
        box.ariaDescription = `vector index ${indexVal} contains another vector`
        box.ariaLabel = `vector index ${indexVal} contains another vector`
      } else {
        box.ariaDescription = `vector index ${indexVal} contains object`//${e.toString()}`
        box.ariaLabel = `vector index ${indexVal} contains object`//${e.toString()}`
      }
      col.appendChild(box);
  
      //creates the arrow element for the vector
      for(let j=0; j < vectorHeight(vector, i + 1) - 3; j++) {
        const arrow = document.createElement('div');
        arrow.className = 'vec-arrow'
        col.appendChild(arrow);
      }
      const val = document.createElement('div');
      val.className = 'down-arrow-box';
      val.textContent = '  ▽';
      val.setAttribute('aria-hidden', 'true');
      col.appendChild(val);
  
      //creates the box containing the value in the element
      const val2 = document.createElement('div');
      val2.className = 'val-box';
      if(typeof e === 'string' || typeof e === 'number' || typeof e === 'boolean') {
        if(typeof e === 'string'){
          val2.textContent = "\"" + e + "\""
        } else {
          val2.textContent = e + '';
        }
        col.appendChild(val2);
      } else if (U.isList(e)) {
        col.appendChild(drawListHTML(e, ancestor+ `:${Number(i)}`, imgID));
      } else if (U.isPair(e)) {
        col.appendChild(drawPairHTML(e, ancestor+ `:${Number(i)}`, imgID));
      } else if (U.isArray(e)) {
        col.appendChild(drawVectorHTML(e, ancestor+ `:${Number(i)}`, imgID));
      } else if (U.isStruct(e)) {
        col.appendChild(drawStructHTML(e, ancestor+ `:${Number(i)}`, imgID));
      }
  
      div.appendChild(col);
    })
    return div
  }
  
  function lengthList(lst: L.List | null, count = 0) {
    if(lst === null) {
      return 0
    }
    if(lst.tail === null) {
      return count + 1
    } else {
      count = count + 1
      return lengthList(lst.tail, count)
    }
  }
  
  function listHeight(list: L.List | null): number {
    let height = 0
    if(U.isList(list)) {
      if(list === null) {
        return 1
      }
      const head = list.head
      if(list.tail === null) {
        if(typeof head === 'string' || typeof head === 'number' || typeof head === 'boolean') {
          height = height + 2 //1
        } else if(U.isList(head)) {
          height = height + listHeight(head) + 1
        } else if(U.isPair(head)) {
          height = height + pairHeight(head)
        } else if(U.isArray(head)) {
          height = height + vectorHeight(head)
        } else if (U.isStruct(head)) {
          height = height + structHeight(head)
          height = height + 3
        }
      } else {
        if(typeof head === 'string' || typeof head === 'number' || typeof head === 'boolean') {
          height = height + listHeight(list.tail) + 1
        } else if(U.isList(head)) {
          height = height + listHeight(head) + listHeight(list.tail) //1
        } else if(U.isPair(head)) {
          height = height + pairHeight(head) + listHeight(list.tail) //1
        } else if(U.isArray(head)) {
          height = height + vectorHeight(head) - 1 + listHeight(list.tail)
        } else if (U.isStruct(head)) {
          height = height + structHeight(head) + listHeight(list.tail)
          height = height + 3
        }
      }
    }
    return height + 1
  }
  
  function keyHandler(key: any, box: HTMLElement, mode: string) {
    let loc = box.id
    //handles checks when key is pressed in a vector
    if(mode === 'vector') {
      if(key === 'ArrowDown') {
        loc = `${loc.substring(0,loc.lastIndexOf(" "))}:0 val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowUp') {
        loc = `${loc.substring(0,loc.lastIndexOf(":"))} val`
        if(document.getElementById(loc) && !document.getElementById(loc)?.className.includes("struct")) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowRight') {
        loc = `${loc.substring(0,loc.lastIndexOf(":"))}:${Number(loc.substring(loc.lastIndexOf(":")+1,loc.lastIndexOf(" ")))+1} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowLeft') {
        loc = `${loc.substring(0,loc.lastIndexOf(":"))}:${Number(loc.substring(loc.lastIndexOf(":")+1,loc.lastIndexOf(" ")))-1} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        } else {
          loc = `${loc.substring(0,loc.lastIndexOf(":"))} val`
          if(document.getElementById(loc) && document.getElementById(loc)?.className.includes("struct")) {
            document.getElementById(loc)?.focus()
          }
        }
      }
      //handles checks in a list when in the first element of a list pair
    } else if (mode === 'struct') {
      if(key === 'ArrowDown') {
        loc = `${loc.substring(0,loc.lastIndexOf(":"))}:${Number(loc.substring(loc.lastIndexOf(":")+1, loc.lastIndexOf(" "))) + 1} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowUp') {
        loc = `${loc.substring(0,loc.lastIndexOf(":"))}:${Number(loc.substring(loc.lastIndexOf(":")+1, loc.lastIndexOf(" "))) - 1} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        } else {
          loc = `${loc.substring(0,loc.lastIndexOf(":"))} val`
          if(document.getElementById(loc) && !document.getElementById(loc)?.className.includes("struct")) {
          document.getElementById(loc)?.focus()
          }
        }
      } else if(key === 'ArrowRight') {
        loc = `${loc.substring(0,loc.lastIndexOf(" "))}:0 val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowLeft') {
        loc = `${loc.substring(0,loc.lastIndexOf(":"))} val`
        if(document.getElementById(loc) && document.getElementById(loc)?.className.includes("struct")) {
          document.getElementById(loc)?.focus()
        }
      }
      //handles checks in a list when in the first element of a list pair
    } else if(loc.includes('val')) {
      if(key === 'ArrowDown') {
        loc = `${loc.substring(0,loc.lastIndexOf(" "))}:0 val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)!.focus()
        }
      } else if(key === 'ArrowUp') {
        loc = `${loc.substring(0,loc.lastIndexOf(":"))} val`
        if(document.getElementById(loc) && !document.getElementById(loc)?.className.includes("struct")) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowRight'){
        loc = `${loc.substring(0,loc.lastIndexOf(" "))} next`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowLeft') {
        loc = `${loc.substring(0,loc.lastIndexOf(":"))}:${Number(loc.substring(loc.lastIndexOf(":")+1,loc.lastIndexOf(" "))) - 1} next`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        } else {
          loc = `${loc.substring(0,loc.lastIndexOf(":"))} val`
          if(document.getElementById(loc) && document.getElementById(loc)?.className.includes("struct")) {
            document.getElementById(loc)?.focus()
          }
        }
      } 
      //handles checks in a list if in the second element of a list pair
    } else if(loc.includes('next')) {
      if(key === 'ArrowDown') {
        loc = `${loc.substring(0,loc.lastIndexOf(" "))}:0 val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowUp') {
        loc = `${loc.substring(0,loc.lastIndexOf(":"))} val`
        if(document.getElementById(loc) && !document.getElementById(loc)?.className.includes("struct")) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowRight') {
        loc = `${loc.substring(0,loc.lastIndexOf(":"))}:${Number(loc.substring(loc.lastIndexOf(":")+1,loc.lastIndexOf(" "))) + 1} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowLeft') {
        loc = `${loc.substring(0,loc.lastIndexOf(" "))} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        } else {
          loc = `${loc.substring(0,loc.lastIndexOf(":"))} val`
          if(document.getElementById(loc) && document.getElementById(loc)?.className.includes("struct")) {
            document.getElementById(loc)?.focus()
          }
        }
      }
    } 
  }
  
  //if variable is given a default value, should always be called with default value outside of the function
  export function drawListHTML(list: L.List | null, ancestor = "0", imgID: number = Math.random()): HTMLDivElement {
    //declares overall html object to be appended to page
    const div = document.createElement('div');
    div.ariaDescription = 'object type list';
    div.tabIndex = 0;
    div.style.position = 'relative';
    
    if(U.isList(list)) {
      const len = lengthList(list);
  
      //loops through the list creating pairs and arrows for each element
      for(let i = 0; i < len; i++) {
        //creates the container for the individual list element and the sub element that contains the list pair
        const col = document.createElement('div');
        col.className = 'vector-style';
        if(i > 0) col.style.position = 'absolute';
        col.style.left = `${105 * i}px`;
        const top = document.createElement('div');
        top.className = 'list-style'
  
        //creates the list pair elements
        for(let j = 0; j < 2; j++) {
          const box = document.createElement('div');
          box.tabIndex = 0;
          box.addEventListener('keydown', (e) => {
            keyHandler(e.key, box, 'list');
          })
          if(j === 0) {
            box.id = `${imgID}:${ancestor}:${i} val`
            if(U.isList(list!.head)) {
              box.ariaDescription = `list pair ${i}, first element contains another list`;
              box.ariaLabel = `list pair ${i}, first element contains another list`;
            } else if(U.isPair(list!.head)) {
              box.ariaDescription = `list pair ${i}, first element contains a pair`;
              box.ariaLabel = `list pair ${i}, first element contains a pair`;
            }else if(U.isArray(list!.head)) {
              box.ariaDescription = `list pair ${i}, first element contains a vector`;
              box.ariaLabel = `list pair ${i}, first element contains a vector`;
            } else {
              box.ariaDescription = `list pair ${i}, first element contains ${list!.head}`;
              box.ariaLabel = `list pair ${i}, first element contains ${list!.head}`;
            }
          } else {
            box.id = `${imgID}:${ancestor}:${i} next`
              box.style.marginLeft = '-2px';
            box.ariaDescription = `list pair ${i}, second element contains a list pair`;
            box.ariaLabel = `list pair ${i}, second element contains a list pair`;
          }
          if(i === len!-1 && j === 1) {
            box.className = 'null-box';
              box.style.marginLeft = '-2px';
            box.ariaDescription = `list pair ${i}, second element contains null`;
            box.ariaLabel = `list pair ${i}, second element contains null`;
          } else {
            box.className = 'list-box';
          }
          top.appendChild(box);
        }
  
        //creates the arrow pointing to the next list element, if there is one
        if(i !== len! - 1) {
          const nextArrow = document.createElement('div');
          nextArrow.className = 'list-arrow';
          const arrowHead = document.createElement('div');
          arrowHead.className = 'arrow-box'
          arrowHead.textContent = '▶'
          top.appendChild(nextArrow);
          top.appendChild(arrowHead);
        }
        col.appendChild(top);
  
        if(list!.tail !== null) {
          //creates the arrow pointing to the contained element
          for(let j = 0; j < listHeight(list!.tail); j++) {
            const arrow = document.createElement('div');
            arrow.className = 'list-arrow-down'
            col.appendChild(arrow);
          }
        } else {
          const arrow = document.createElement('div');
          arrow.className = 'list-arrow-down'
          col.appendChild(arrow);
        }
        
        //creates the container for the value contained in the first element of a list pair
        let el = list!.head
        const val = document.createElement('div');
        val.className = 'val-box';
        val.textContent = '▼'
        col.appendChild(val);
        const val2 = document.createElement('div');
        val2.className = 'val-box';
        if(U.isNull(el)) {
          val2.textContent = 'null';
          col.appendChild(val2)
        } else if(typeof el === 'string' || typeof el === 'number' || typeof el === 'boolean') {
          if(typeof el === 'string'){
            val2.textContent = "\"" + el + "\""
          } else {
            val2.textContent = el + '';
          }
          col.appendChild(val2);
        } else if(el !== null && U.isList(el)) {
          col.appendChild(drawListHTML(el, ancestor+ `:${Number(i)}`, imgID));
        } else if (U.isPair(el)) {
          col.appendChild(drawPairHTML(el, ancestor+ `:${Number(i)}`, imgID));
        } else if (U.isArray(el)) {
          col.appendChild(drawVectorHTML(el, ancestor+ `:${Number(i)}`, imgID));
        } else if (U.isStruct(el)) {
          col.appendChild(drawStructHTML(el, ancestor+ `:${Number(i)}`, imgID));
        }
        
        //iterates the list
        list = list!.tail;
        div.appendChild(col);
      }
    }
    return div;
  }
  /*
  Catppuccin for VSCode 
by Catppuccin
  Omni Theme 
by Rocketseat
  Panda Theme 
by Panda Theme
  Monokai Night Theme 
by Fabio Spampinato
  Rosé Pine
by Rosé Pine
  Pink-Cat-Boo Theme
by Fiona Fan
  Black
by Jaakko
  Lunar Pink Theme
by Nícolas D. Schmidt
  Synthwave x Fluoromachine
by webrender
  Nebula Pandas
by GokturkSM
  */

  function pairHeight(pair: L.Pair) {
    let height = 3
    const fst = pair.fst
    const snd = pair.snd
    
    //height of pair.snd
    if(typeof snd === 'string' || typeof snd === 'number' || typeof snd === 'boolean' ) {
      height = height + 1
    } else if (U.isList(snd)) {
      height = height + listHeight(snd) + 1
    } else if (U.isPair(snd)) {
      height = height + pairHeight(snd)
    } else if (U.isArray(snd)) {
      height = height + vectorHeight(snd)
    } else if (U.isStruct(snd)) {
      height = height + structHeight(snd)
    }
  
    //height of pair.fst
    if(typeof fst === 'string' || typeof fst === 'number' || typeof fst === 'boolean' ) {
      height = height + 1
    } else if (U.isList(fst)) {
      height = height + listHeight(fst) + 1
    } else if (U.isPair(fst)) {
      height = height + pairHeight(fst)
    } else if (U.isArray(fst)) {
      height = height + vectorHeight(fst)
    } else if (U.isStruct(fst)) {
      height = height + structHeight(fst)
    }
  
    return height
  }
  
  export function drawPairHTML(pair: L.Pair, ancestor = "0", imgID: number = Math.random()): HTMLDivElement {
    //Container for html elements
    const div = document.createElement('div');
    div.ariaLabel = 'object type pair';
    div.tabIndex = 0;
    div.style.position = 'relative';
  
    //loops through the pair, making the visualization pieces for each element
    for(let k = 0; k < 2; k++) {
      
      //container for all the html elements for one pair element
      const col = document.createElement('div');
      col.className = 'vector-style';
      if (k > 0) col.style.position = 'absolute';
      col.style.left = `${30 * k}px`
      //col.style.top = '20px'
  
      //creates the elements for the box elements of the pair
      const box = document.createElement('div');
      box.className = 'vector-box';
      box.id = `${imgID}:` + ancestor + `:` + k + ` val`
      //box.role = 'img'
      box.tabIndex = 0;
      box.addEventListener('keydown', (e) => {
        keyHandler(e.key, box, 'vector');
      })
      if(k > 0) {
        if(U.isList(pair!.snd)) {
          box.ariaDescription = `non-list pair element 2 contains list`;
          box.ariaLabel = `non-list pair element 2 contains list`;
        } else if(U.isPair(pair!.snd)) {
          box.ariaDescription = `non-list pair element 2 contains another pair`;
          box.ariaLabel = `non-list pair element 2 contains another pair`;
        } else if(U.isArray(pair!.snd)) {
          box.ariaDescription = `non-list pair element 2 contains vector`;
          box.ariaLabel = `non-list pair element 2 contains vector`;
        } else if(U.isNull(pair!.snd)) {
          box.ariaDescription = `non-list pair element 2 contains null`;
          box.ariaLabel = `non-list pair element 2 contains null`;
        } else {
          box.ariaDescription = `non-list pair element 2 contains ${pair!.snd}`;
          box.ariaLabel = `non-list pair element 2 contains ${pair!.snd}`;
        }
        
        // box.ariaDescription = `non-list pair element 2, second element contains ${k === 0? pair.fst : pair.snd}`
        // box.ariaLabel = `non-list pair element 2, second element contains ${k === 0? pair.fst : pair.snd}`
      } else {
        if(U.isList(pair!.fst)) {
          box.ariaDescription = `non-list pair element 1 contains list`;
          box.ariaLabel = `non-list pair element 1 contains list`;
        } else if(U.isPair(pair!.fst)) {
          box.ariaDescription = `non-list pair element 1 contains another pair`;
          box.ariaLabel = `non-list pair element 1 contains another pair`;
        }else if(U.isArray(pair!.fst)) {
          box.ariaDescription = `non-list pair element 1 contains vector`;
          box.ariaLabel = `non-list pair element 1 contains vector`;
        } else if(U.isStruct(pair!.fst)) {
          box.ariaDescription = `non-list pair element 1 contains struct`;
          box.ariaLabel = `non-list pair element 1 contains struct`;
        } else if(U.isNull(pair!.fst)) {
          box.ariaDescription = `non-list pair element 1 contains null`;
          box.ariaLabel = `non-list pair element 1 contains null`;
        } else {
          box.ariaDescription = `non-list pair element 1 contains ${pair!.fst}`;
          box.ariaLabel = `non-list pair element 1 contains ${pair!.fst}`;
        }
        
        // box.ariaDescription = `non-list pair element 1, first element contains ${k === 0? pair.fst : pair.snd}`
        // box.ariaLabel = `non-list pair element 1, first element contains ${k === 0? pair.fst : pair.snd}`
      }
      col.appendChild(box);
      const snd = pair.snd
  
      let height = 1
      if(k === 0) {
        if(typeof snd === 'string' || typeof snd === 'number' || typeof snd === 'boolean' ) {
          height = height + 1
        } else if (U.isList(snd)) {
          height = height + listHeight(snd) + 1
        } else if (U.isPair(snd)) {
          height = height + pairHeight(snd)
        } else if (U.isArray(snd)) {
          height = height + vectorHeight(snd)
        } else if (U.isStruct(snd)) {
          height = height + structHeight(snd)
        } else {
          height = height + 1
        }
      }
  
      //creates the arrow element for the pair
      for(let j=0; j < height; j++) {
        const arrow = document.createElement('div');
        arrow.className = 'list-arrow-down'
        col.appendChild(arrow);
      }
  
      const e = k === 0? pair.fst : pair.snd
      const val = document.createElement('div');
      val.className = 'val-box';
      val.textContent = '▼\n';
      if (e === pair.snd) {
        col.style.marginLeft = '-2px'
      }
      col.appendChild(val);
      const val2 = document.createElement('div');
      val2.className = 'val-box';
      //creates the box containing the value in the element
      if(typeof e === 'string' || typeof e === 'number' || typeof e === 'boolean') {
        if(typeof e === 'string'){
          val2.textContent = "\"" + e + "\""
        } else {
          val2.textContent = e + '';
        }
        col.appendChild(val2);
      } else if (U.isList(e)) {
        col.appendChild(drawListHTML(e, ancestor+ `:${Number(k)}`, imgID));
      } else if (U.isPair(e)) {
        col.appendChild(drawPairHTML(e, ancestor+ `:${Number(k)}`, imgID));
      } else if (U.isArray(e)) {
        col.appendChild(drawVectorHTML(e, ancestor+ `:${Number(k)}`, imgID));
      } else if (U.isStruct(e)) {
        col.appendChild(drawStructHTML(e, ancestor+ `:${Number(k)}`, imgID));
      } if(U.isNull(e)) {
        val2.textContent = 'null';
        col.appendChild(val2)
      } else {
        div.appendChild(col);
      }
    }
    return div;
  }
  
  
  function structHeight(struct: L.Struct) : number {
    let height = 1
    let count = 0
    for (let thing in struct) {
      thing = struct[thing]

      if (count > 1) {
        if(typeof thing === 'string' || typeof thing === 'number' || typeof thing === 'boolean' ) {
          height = height + 4
        } else if (U.isList(thing)) {
          height = height + listHeight(thing)
        } else if (U.isPair(thing)) {
          height = height + pairHeight(thing)
        } else if (U.isArray(thing)) {
          height = height + vectorHeight(thing)
        } else if (U.isStruct(thing)) {
          height = height + structHeight(thing)
        } else {
          height = height + 3
        }
      }
      count = count + 1
    }
    return height
  }
  
  export function drawStructHTML(struct: L.Struct, ancestor = "0", imgID: number = Math.random()): HTMLDivElement {
    const div = document.createElement('div');
    div.tabIndex = 0;
    
  
    const col = document.createElement('div');
    //col.style.width = '';
      ///col.style.left = `${30}px`
    const col2 = document.createElement('div');
      col2.style.margin = "-3"
      //col.style.position = 'absolute'
      col2.style.left = `${30}px`
  
    let countThings = 0;
    const numberOfElements = Object.keys(struct).length;
      
    for (const thing in struct) {
      countThings++;
      if(countThings > 2) {
      const row = document.createElement('div');
      row.id = "struct-row" + countThings
      row.style.left = `${30}px`
      row.style.display = 'flex'
      row.style.flexDirection = 'row'
  
      if(countThings !== numberOfElements) {
        const line = document.createElement('div');
        line.className = 'struct-line';
        row.appendChild(line!);
      }
  
      let t = struct[thing]
      let s = thing.toString() + "      "
  
      const box = document.createElement('div');
        box.id = `${imgID}:` + ancestor + `:` + `${countThings-3} val`
        box.className = 'struct-box';
        box.tabIndex = 0;
        if(U.isList(t)) {
          box.ariaDescription = `struct element ${countThings-3} contains list`;
          box.ariaLabel = `struct element ${countThings-3} contains list`;
        } else if(U.isPair(t)) {
          box.ariaDescription = `struct element ${countThings-3} contains pair`;
          box.ariaLabel = `struct element ${countThings-3} contains pair`;
        }else if(U.isArray(t)) {
          box.ariaDescription = `struct element ${countThings-3} contains vector`;
          box.ariaLabel = `struct element ${countThings-3} contains vector`;
        } else if(U.isStruct(t)) {
          box.ariaDescription = `struct element ${countThings-3} contains struct`;
          box.ariaLabel = `struct element ${countThings-3} contains struct`;
        } else if (t.toString().includes("L.mkStruct(t, fieldNames, args);")) {
          box.ariaDescription = `struct element ${countThings-3} contains empty struct`;
          box.ariaLabel = `struct element ${countThings-3} contains empty struct`;
        } else {
          box.ariaDescription = `struct element ${countThings-3} contains ${t}`;
          box.ariaLabel = `struct element ${countThings-3} contains ${t}`;
        }
        if(countThings === numberOfElements) {
          box.style.borderLeft = '6px solid black'
        } else {
          //box.style.marginBottom = '50px'
        }
        box.innerHTML = s
        
        row.appendChild(box);
  
      const nextArrow = document.createElement('div');
        nextArrow.className = 'list-arrow';
      const arrowHead = document.createElement('div');
        arrowHead.className = 'arrow-box'
        arrowHead.textContent = '▶'
      const miniDiv = document.createElement('div');
        miniDiv.className = 'list-style'
        miniDiv.appendChild(nextArrow)
        miniDiv.appendChild(arrowHead)
        row.appendChild(miniDiv);
  
        let HTMLVal = document.createElement('div'); // div to hold element to be drawn
        let tHeight = 0
        if(U.isNull(t)) {
          const val2 = document.createElement('div');
            val2.className = 'val-box';
            val2.style.paddingTop = '5px'
            val2.innerHTML = "null";
          HTMLVal = val2
          tHeight = 50
        } else if(typeof t === 'string' || typeof t === 'number' || typeof t === 'boolean') {
          const val2 = document.createElement('div');
            val2.className = 'val-box';
            val2.innerHTML = t.toString();
          if(typeof t === 'string') {
            val2.innerHTML = '"' + t + '"'
          }
            val2.style.paddingTop = '5px'
            val2.style.whiteSpace = 'noWrap';
          HTMLVal = val2;
          tHeight = 50
        } else if (U.isList(t)) {
          HTMLVal = drawListHTML(t, ancestor+ `:${countThings-3}`, imgID);
          tHeight = listHeight(t) + 50
        } else if (U.isPair(t)) {
          HTMLVal = drawPairHTML(t, ancestor+ `:${countThings-3}`, imgID);
          tHeight = pairHeight(t) + 50
        } else if (U.isArray(t)) {
          HTMLVal = drawVectorHTML(t, ancestor+ `:${countThings-3}`, imgID);
          tHeight = vectorHeight(t) + 50
        } else if (U.isStruct(t)) {
          HTMLVal = drawStructHTML(t, ancestor+ `:${countThings-3}`, imgID);
          tHeight = structHeight(t) + 50
        } else if (t.toString().includes("L.mkStruct(t, fieldNames, args);")) { //leaf node
          const box = document.createElement('div');
          box.id = "empty struct"
          box.className = 'struct-box';
          
          box.tabIndex = 0;
          box.ariaDescription = `empty value`
          box.ariaLabel = `empty value`
          box.id = `${imgID}:${ancestor}:0`
          
          box.innerHTML = "empty struct"
          HTMLVal = box;
          tHeight = 50
        }
        box.style.marginBottom = `${tHeight}px`
        box.addEventListener('keydown', (e) => {
          keyHandler(e.key, box, 'struct');
        })

      row.appendChild(HTMLVal)
      col.appendChild(row)
      }
    }

  
    //col.style.borderLeft = '2px solid black
    div.appendChild(col);
    div.appendChild(col2)
  
    return div
  }