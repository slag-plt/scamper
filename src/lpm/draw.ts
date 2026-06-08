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
        height = height + structHeight(e)
        height = height + 1
      }
    }
    return height + 3
  }
  
  export function drawVectorHTML(vector: L.Vector, nesting = 0, parent = 0, imgID: number = Math.random()): HTMLDivElement {
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
        //col.style.position = 'absolute';
      }
      col.style.left = `${30 * vector.indexOf(e)}px`
  
      //creates the box elements of the vector
      const box = document.createElement('div');
      const index = document.createElement('div');
      const indexVal = vector.indexOf(e).toString();
      index.className = 'index-box';
      index.textContent = indexVal;
      col.appendChild(index);
      box.className = 'vector-box';
      box.tabIndex = 0;
      box.id = `${nesting}:${i}:${parent}:${imgID} val`
      box.addEventListener('keydown', (e) => {
        keyHandler(e.key, box, 'vector', imgID);
      })
      if(U.isList(e)) {
        box.ariaDescription = `vector index ${indexVal} contains a list`
        box.ariaLabel = `vector index ${indexVal} contains a list`
      }if(U.isPair(e)) {
        box.ariaDescription = `vector index ${indexVal} contains a pair`
        box.ariaLabel = `vector index ${indexVal} contains a pair`
      } else if(U.isArray(e)) {
        box.ariaDescription = `vector index ${indexVal} contains a vector`
        box.ariaLabel = `vector index ${indexVal} contains a vector`
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
      } else if (U.isPair(e)) {
        if(U.isList(e)) {
          col.appendChild(drawListHTML(e, nesting + 1, i, imgID));
        } else {
          col.appendChild(drawPairHTML(e, nesting + 1, i, imgID));
        }
      } else if (U.isArray(e)) {
        col.appendChild(drawVectorHTML(e, nesting + 1, i, imgID));
      } else if (U.isStruct(e)) {
        col.appendChild(drawStructHTML(e))
      }
  
      div.appendChild(col);
    })
    return div
  }
  
  function lengthList(lst: L.List | null, count = 0) {
    if(lst === null) {
      return 0
    }
    if(lst.snd === null) {
      return count + 1
    } else {
      count = count + 1
      return lengthList(lst.snd, count)
    }
  }
  
  function listHeight(list: L.List | null): number {
    let height = 0
    if(U.isList(list)) {
      if(list === null) {
        return 1
      }
      const fst = list.fst
      if(list.snd === null) {
        if(typeof fst === 'string' || typeof fst === 'number' || typeof fst === 'boolean') {
          height = height + 2 //1
        } else if(U.isList(fst)) {
          height = height + listHeight(fst) + 1
        } else if(U.isPair(fst)) {
          height = height + pairHeight(fst)
        } else if(U.isArray(fst)) {
          height = height + vectorHeight(fst)
        } else if (U.isStruct(fst)) {
          height = height + structHeight(fst)
        }
      } else {
        if(typeof fst === 'string' || typeof fst === 'number' || typeof fst === 'boolean') {
          height = height + listHeight(list.snd) + 1
        } else if(U.isList(fst)) {
          height = height + listHeight(fst) + listHeight(list.snd) //1
        } else if(U.isPair(fst)) {
          height = height + pairHeight(fst) + listHeight(list.snd) //1
        } else if(U.isArray(fst)) {
          height = height + vectorHeight(fst) - 1 + listHeight(list.snd)
        } else if (U.isStruct(fst)) {
          height = height + structHeight(fst) + listHeight(list.snd)
          height = height + 3
        }
      }
    }
    return height + 1
  }
  
  function keyHandler(key: any, box: HTMLElement, mode: string, imgID: number) {
    let loc = box.id
    //handles checks when key is pressed in a vector
    if(mode === 'vector') {
      if(key === 'ArrowDown') {
        loc = `${Number(loc[0] ) + 1}:0:${loc[2]}:${imgID} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowRight') {
        loc = `${loc[0]}:${Number(loc[2]) + 1}:${loc[4]}:${imgID} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowLeft') {
        loc = `${loc[0]}:${Number(loc[2]) - 1}:${loc[4]}:${imgID} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      }
      //handles checks in a list when in the first element of a list pair
    } else if(loc.includes('val')) {
      if(key === 'ArrowDown') {
        loc = `${Number(loc[0]) + 1}:0:${loc[2]}:${imgID} val`
        if(document.getElementById(loc)) {
          console.log('testing')
          document.getElementById(loc)!.focus()
        }
      } else if(key === 'ArrowRight'){
        loc = `${loc[0]}:${loc[2]}:${loc[4]}:${imgID} next`
        if(document.getElementById(loc)) {
          console.log('testing')
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowLeft') {
        loc = `${loc[0]}:${Number(loc[2]) - 1}:${loc[4]}:${imgID} next`
        if(document.getElementById(loc)) {
          console.log('testing')
          document.getElementById(loc)?.focus()
        }
      } 
      //handles checks in a list if in the second element of a list pair
    } else if(loc.includes('next')) {
      if(key === 'ArrowDown') {
        loc = `${Number(loc[0] + 1)}:0:${loc[2]}:${imgID} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowRight') {
        loc = `${loc[0]}:${Number(loc[2]) + 1}:${loc[4]}:${imgID} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      } else if(key === 'ArrowLeft') {
        loc = `${loc[0]}:${loc[2]}:${loc[4]}:${imgID} val`
        if(document.getElementById(loc)) {
          document.getElementById(loc)?.focus()
        }
      }
    }
  }
  
  //if variable is given a default value, should always be called with default value outside of the function
  export function drawListHTML(list: L.List | null, nesting = 0, parent = 0, imgID: number = Math.random()): HTMLDivElement {
    //declares overall html object to be appended to page
    const div = document.createElement('div');
    div.ariaDescription = 'object type list';
    //div.tabIndex = 0;
    div.style.position = 'relative';
  
    if(U.isList(list)) {
      if(list === null) {
        const div = document.createElement('div')
        div.className = 'null-box'
        return div
      }

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
            console.log('testing')
            keyHandler(e.key, box, 'list', imgID);
          })
          if(j === 0) {
            box.id = `${nesting}:${i}:${parent}:${imgID} val`
            if(U.isList(list!.fst)) {
              box.ariaDescription = `list pair ${i}, nesting level ${nesting} first element contains another list`;
              box.ariaLabel = `list pair ${i}, nesting level ${nesting} first element contains another list`;
            } else if(U.isArray(list!.fst)) {
              box.ariaDescription = `list pair ${i}, nesting level ${nesting} first element contains a vector`;
              box.ariaLabel = `list pair ${i}, nesting level ${nesting} first element contains a vector`;
            } else {
              box.ariaDescription = `list pair ${i}, nesting level ${nesting} first element contains ${list!.fst}`;
              box.ariaLabel = `list pair ${i}, nesting level ${nesting} first element contains ${list!.fst}`;
            }
          } else {
            box.id = `${nesting}:${i}:${parent}:${imgID} next`
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
  
        if(list!.snd !== null) {
          //creates the arrow pointing to the contained element
          for(let j = 0; j < listHeight(list!.snd); j++) {
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
        let el = list!.fst
        const val = document.createElement('div');
        val.className = 'val-box';
        val.textContent = '▼'
        col.appendChild(val);
        const val2 = document.createElement('div');
        val2.className = 'val-box';
        if(el === null) {
          val2.textContent = 'null';
        } else if(typeof el === 'string' || typeof el === 'number' || typeof el === 'boolean') {
          if(typeof el === 'string'){
            val2.textContent = "\"" + el + "\""
          } else {
            val2.textContent = el + '';
          }
          col.appendChild(val2);
        } else if(el !== null && U.isList(el)) {
          col.appendChild(drawListHTML(el, nesting + 1, i, imgID));
        } else if (U.isPair(el)) {
          col.appendChild(drawPairHTML(el, nesting + 1, i, imgID));
        } else if (U.isArray(el)) {
          col.appendChild(drawVectorHTML(el, nesting + 1, i, imgID));
        } else if (U.isStruct(el)) {
          col.appendChild(drawStructHTML(el))
        }
        
        //iterates the list
        list = list!.snd;
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

  // //ASCII
  // function drawPair(pair: any): any {
  //   let str = ''
  //   let fst = pair.fst
  //   let snd = pair.snd
  //   if(typeof fst === 'string' || typeof fst === 'number' || typeof fst === 'boolean') {
  //     str = str + '{ ' + fst
  //   } else if(Value.isPair(fst)) {
  //     str = str + drawPair(fst) + ' }'
  //   } else if(Value.typeOf(fst) === 'list') {
  //     str = str + drawList(fst) + ' }'
  //   }
  //   str = str + ' }-{ '
  //   if(typeof snd === 'string' || typeof snd === 'number' || typeof snd === 'boolean') {
  //     str = str + snd + ' }'
  //   } else if(Value.isPair(snd)) {
  //     str = str + drawPair(snd) + ' }'
  //   } else if(Value.typeOf(snd) === 'list') {
  //     str = str + drawList(snd) + ' }'
  //   }
  //   return str
  // }
  
  function pairHeight(pair: L.Pair) {
    let height = 3
    const fst = pair.fst
    const snd = pair.snd
    
    //height of pair.snd
    if(typeof snd === 'string' || typeof snd === 'number' || typeof snd === 'boolean' ) {
      height = height + 1
    } else if (U.isList(snd)) {
      height = height + listHeight(snd)
    } else if (U.isPair(snd)) {
      height = height + pairHeight(snd)
    } else if (U.isArray(snd)) {
      height = height + vectorHeight(snd)
    } else if (U.isStruct(snd)) {
      height = structHeight(snd)
    }
  
    //height of pair.fst
    if(typeof fst === 'string' || typeof fst === 'number' || typeof fst === 'boolean' ) {
      height = height + 1
    } else if (U.isList(fst)) {
      height = height + listHeight(fst)
    } else if (U.isPair(fst)) {
      height = height + pairHeight(fst)
    } else if (U.isArray(fst)) {
      height = height + vectorHeight(fst) - 1
    } else if (U.isStruct(fst)) {
      height = structHeight(fst)
    }
  
    return height
  }
  
  export function drawPairHTML(pair: L.Pair, nesting = 0, parent = 0, imgID: number = Math.random()): HTMLDivElement {
    //Container for html elements
    const div = document.createElement('div');
    div.ariaLabel = 'object type pair';
    //div.tabIndex = 0;
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
      box.id = `${nesting}:${k}:${parent}:${imgID} val`
      //box.role = 'img'
      box.tabIndex = 0;
      box.addEventListener('keydown', (e) => {
        keyHandler(e.key, box, 'vector', imgID);
      })
      if(k > 0) {
        box.ariaDescription = `non-list pair element 2, second element contains ${k === 0? pair.fst : pair.snd}`
        box.ariaLabel = `non-list pair element 2, second element contains ${k === 0? pair.fst : pair.snd}`
      } else {
        box.ariaDescription = `non-list pair element 1, first element contains ${k === 0? pair.fst : pair.snd}`
        box.ariaLabel = `non-list pair element 1, first element contains ${k === 0? pair.fst : pair.snd}`
      }
      col.appendChild(box);
      let snd = pair.snd
  
      let height = 1
      if(k === 0) {
        if(typeof snd === 'string' || typeof snd === 'number' || typeof snd === 'boolean' ) {
          height = height + 1
        } else if (U.isList(snd)) {
          height = height + listHeight(snd)
        } else if (U.isPair(snd)) {
          height = height + pairHeight(snd)
        } else if (U.isArray(snd)) {
          height = height + vectorHeight(snd)
        } else if (U.isStruct(snd)) {
          height = structHeight(snd)
        }
      }
  
      //creates the arrow element for the pair
      for(let j=0; j < height; j++) {
        const arrow = document.createElement('div');
        arrow.className = 'list-arrow-down'
        col.appendChild(arrow);
      }
  
      let e = k === 0? pair.fst : pair.snd
      const val = document.createElement('div');
      val.className = 'val-box';
      val.textContent = '▼\n';
      if (e === pair.snd) {
        col.style.marginLeft = '-2px'
      }
      col.appendChild(val);
      let val2 = document.createElement('div');
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
        col.appendChild(drawListHTML(e, nesting + 1, k, imgID));
      } else if (U.isPair(e)) {
        col.appendChild(drawPairHTML(e, nesting + 1, k, imgID));
      } else if (U.isArray(e)) {
        col.appendChild(drawVectorHTML(e, nesting + 1, k, imgID));
      } else if (U.isStruct(e)) {
        col.appendChild(drawStructHTML(e))
      }
       div.appendChild(col);
    }
    return div;
  }
  
  
  function structHeight(struct: L.Struct) : number {
    let height = 0
    for (let thing in struct) {
      thing = struct[thing]
      if(typeof thing === 'string' || typeof thing === 'number' || typeof thing === 'boolean' ) {
        height = height + 3
      } else if (U.isPair(thing)) {
        height = height + listHeight(thing)
      } else if (U.isPair(thing)) {
        height = height + pairHeight(thing)
      } else if (U.isArray(thing)) {
        height = height + vectorHeight(thing)
      } else if (U.isStruct(thing)) {
        height = height + structHeight(thing)
      }
    }
    return height
  }
  
  export function drawStructHTML(struct: L.Struct) {
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
  
      const row = document.createElement('div');
      row.id = "struct-row" + countThings
      row.style.left = `${30}px`
      row.style.display = 'flex'
      row.style.flexDirection = 'row'
  
      if(countThings > 1 && countThings !== numberOfElements) {
        const line = document.createElement('div');
        line.className = 'struct-line';
        row.appendChild(line!);
      }
  
      let t = struct[thing]
      let s = thing.toString() + "      "
  
      const box = document.createElement('div');
        box.id = "struct-box"
        box.className = 'struct-box';
        box.tabIndex = 0;
        box.ariaDescription = `no`
        box.ariaLabel = `no`
        if(countThings === numberOfElements) {
          box.style.borderLeft = '6px solid black'
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
        
        if(typeof t === 'string' || typeof t === 'number' || typeof t === 'boolean') {
          const val2 = document.createElement('div');
            val2.className = 'val-box';
            val2.innerHTML = t.toString();
          if(typeof t === 'string') {
            val2.innerHTML = '\"' + t.toString() + '\"'
          }
            val2.style.paddingTop = '5px'
            val2.style.whiteSpace = 'noWrap';
          HTMLVal = val2;
        } else if (U.isList(t)) {
          HTMLVal = drawListHTML(t);
        } else if (U.isPair(t)) {
          HTMLVal = drawPairHTML(t);
        } else if (U.isArray(t)) {
          HTMLVal = drawVectorHTML(t);
        } else if (U.isStruct(t)) {
          HTMLVal = drawStructHTML(t);
        }
  
      
      row.appendChild(HTMLVal)
      col.appendChild(row)
      
    }
  
    if(countThings === 0) {
      console.log("LEAF")
      console.log(struct)
      const row = document.createElement('div');
      row.style.left = `${30}px`
      row.style.display = 'flex'
      row.style.flexDirection = 'row'
  
      const box = document.createElement('div');
        box.id = "empty struct"
        box.className = 'struct-box';
        
        box.tabIndex = 0;
        box.ariaDescription = `no`
        box.ariaLabel = `no`
        
        box.innerHTML = "empty struct"
        
        row.appendChild(box)
        col.appendChild(row)
    }
  
  
    //col.style.borderLeft = '2px solid black
    div.appendChild(col);
    div.appendChild(col2)
  
    return div
  }

  /*
  function draw (): void {
    let envState = this.state
    let initialLibNum = 0

    this.builtinLibs.forEach(l => {
      initialLibNum += l.lib.length
    })

    if(envState != undefined){

      //grabs bounded values from the environment
      let bounded = envState.getBoundsEnv(initialLibNum)
      
      //grabs the stack
      let stack = envState.getStack()
      console.log(stack)
      //if the stack is empty (if we are not inside the gray tracing box) we visualize the entire bounded variables "list"
      if(!stack[0]) {

        //and bounded variables exist
        if(bounded != undefined && bounded.length > 0) {
          
          //environment begin line
          let div1 = document.createElement('div')
          div1.ariaLabel = "Begin environment"
          div1.ariaDescription = "Begin environment"
          div1.textContent = "------------------------------~"
          div1.tabIndex = 0
          div1.addEventListener('keydown', (event) => {
            if(event.key === 'j' && event.ctrlKey) {
              if(this.jumpToList![this.jumpToList!.indexOf(div1) + 1]) {
                this.jumpToList![this.jumpToList!.indexOf(div1) + 1].focus()
             }
            }
          })
          renderToDraw(this.display, div1)
          this.jumpToList?.push(div1)

          // parallel arrays for keeping divs and their names
          let list_names: String[] = [];
          let list_div: HTMLElement[] = [];

          //for each bounded variable
          bounded?.forEach(([id, value]) => {
            let strVal: any = value?.toString()

            let HTMLVal: any = ''
            let ariaType = ""
            let structName = false;

            //typecheck the variable(s) and convert to string or HTML elements
            if (!strVal || !value) {
              return;
            }
            if (typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean' || strVal === "0") {
              strVal = strVal
              if (typeof value === 'string') {
                HTMLVal = "\"" + value + "\""
              } else {
                HTMLVal = value.toString()
              }
              ariaType = typeof value
            } else if (Value.typeOf(value) === 'vector') {
              strVal = drawVector(value) + ' Vetcor Height ' + (vectorHeight(value) + 1)
              HTMLVal = drawVectorHTML(value)
              ariaType = "vector"
            } else if (Value.typeOf(value) === 'list') {
              strVal = drawList(value) + ' List Height == ' + (listHeight(value) + 1)
              HTMLVal = drawListHTML(value)
              ariaType = "list"
            } else if (Value.isPair(value)) {
              strVal = drawPair(value)
              HTMLVal = drawPairHTML(value)
              ariaType = "pair"
            } else if (Value.isFunction(value)) {
              strVal = ("PROCEDURE")
              ariaType = "procedure"
              HTMLVal = "PROCEDURE"
            } else if (Value.isStruct(value)) {
              HTMLVal = drawStructHTML(value)
              ariaType = 'struct'
              //console.log("STRUCT")
              //console.log(value[0])
              structName = value[0];
            } else {
              console.log("Found none for type " + Value.typeOf(value))
            }

            //make mini div for name and arrow
            let miniDiv = document.createElement('div')
            miniDiv.textContent = id + ' → '
            miniDiv.style.whiteSpace = 'nowrap'
            
            // make div to be drawn later
            let div = document.createElement('div')
            
            div.style.display = 'flex'
            div.ariaLabel = id + " points to " + ariaType
            div.ariaDescription = id + " points to " + ariaType
            div.appendChild(miniDiv)
            div.append(HTMLVal)
            this.jumpToList!.push(HTMLVal)
            div.addEventListener('keydown', (event) => {
              if (event.key === 'j' && event.ctrlKey) {
                if (this.jumpToList![this.jumpToList!.indexOf(HTMLVal) + 1]) {
                  this.jumpToList![this.jumpToList!.indexOf(HTMLVal) + 1].focus()
                }
              }
            })
            //console.log(structName + id)
            if(structName) {
                //console.log(list_names)
                //console.log(list_div)
              for(let i = 0; i < list_names.length; i++) {
                
                if(list_names[i].startsWith(structName)) {
                  list_names.splice(i, 1);
                  list_div.splice(i,1);
                }
              }
            }
            list_names.push(id);
            list_div.push(div)
            //console.log(this.jumpToList)
          })
          list_div.forEach(e => renderToDraw(this.display, e) );
          
          //environment end line
          let div2 = document.createElement('div')
          div2.ariaLabel = "End environment"
          div2.ariaDescription = "End environment"
          div2.textContent = "------------------------------~"
          div2.tabIndex = 0
          renderToDraw(this.display, div2)
        }
      }

      let stackString;
      let stackHTML;

      //if there is anything in the stack ( we are inside the gray tracing box)
      if(stack[0]) {
        //console.log(stack[0])
        //convert to string (probs not used)
        stackString = stack[stack.length - 1]?.toString()

        //type check and convert to string or HTML element
        if(typeof stack[0] != 'string' && typeof stack[0] != 'number' && typeof stack[0] != 'boolean' || stack[0] === 0) {
          if(stack[0] != undefined && Value.typeOf(stack[0]) === 'vector') {
            stackString = drawVector(stack[0])
            stackHTML = drawVectorHTML(stack[0])
          } else if (stack[0] != undefined && Value.typeOf(stack[0]) === 'list') {
            stackString = drawList(stack[0])
            stackHTML = drawListHTML(stack[0])
          } else if (stack[0] != undefined && Value.isPair(stack[0])) {
            stackString = drawPair(stack[0])
            stackHTML = drawPairHTML(stack[0])
          } else if (stack[0] != undefined && Value.isFunction(stack[0])) {
            //@ts-ignore
            if(stack[0].name) {
              //@ts-ignore
              if(stack[0].name === 'cons') {
                let last: any = stack[stack.length - 1]
                if(last.snd === null) {
                  stackString = drawList(Value.mkList(last.fst))
                  stackHTML = drawListHTML(Value.mkList(last.fst))
                } else if(last.snd.isList) {
                  stackString = drawList(Value.mkPair(last.fst, last.snd))
                  stackHTML = drawListHTML(Value.mkPair(last.fst, last.snd))
                } else {
                  stackString = drawPair(Value.mkPair(last.fst, last.snd))
                  stackHTML = drawPairHTML(Value.mkPair(last.fst, last.snd))
                }

                //attempt at drawing map (ignore)
                //@ts-ignore
              } else if(stack[0].name === 'map') {
                //forEachstack.push(Value.mkList)
                console.log("mapping")
              }
            } else {//catch
              stackString = ("PROCEDURE")
              console.log(stackString)
            }
          }
        }
        if(stackHTML){//if there is an element, then append it
          this.appendToCurrentTrace(stackHTML)
        }
      }
    }
    
  }
    */