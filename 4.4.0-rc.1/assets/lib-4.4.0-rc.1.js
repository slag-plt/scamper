const __vite__mapDeps=(i,m=__vite__mapDeps,d=(m.f||(m.f=["assets/fs-43.4.0-rc.1.js","assets/fs-42.4.0-rc.1.js","assets/fs-4.4.0-rc.1.js","assets/opfs-writer-4.4.0-rc.1.js"])))=>i.map(i=>d[i]);
import{i as e,n as t,t as n}from"./rolldown-runtime-4.4.0-rc.1.js";import{$ as r,$i as i,$n as a,$t as o,A as s,Ai as c,An as l,Ar as u,At as d,B as f,Bi as p,Bn as m,Br as h,Bt as g,C as _,Ci as v,Cn as y,Cr as b,Ct as x,D as S,Di as C,Dn as w,Dr as T,Dt as E,E as ee,Ei as te,En as D,Er as O,Et as k,F as ne,Fi as A,Fn as re,Fr as ie,Ft as ae,G as oe,Gi as se,Gn as j,Gr as ce,Gt as le,H as ue,Hi as de,Hn as fe,Hr as pe,Ht as me,I as he,Ii as ge,In as _e,Ir as ve,It as ye,J as be,Ji as xe,Jn as Se,Jt as Ce,K as we,Ki as Te,Kn as Ee,Kr as De,Kt as Oe,L as ke,Li as Ae,Ln as je,Lr as M,Lt as Me,M as Ne,Mi as Pe,Mn as Fe,Mr as N,Mt as Ie,N as Le,Ni as Re,Nn as ze,Nr as P,Nt as Be,O as Ve,Oi as He,On as Ue,Or as We,Ot as Ge,P as Ke,Pi as qe,Pn as Je,Pr as Ye,Pt as Xe,Q as Ze,Qi as Qe,Qn as $e,Qr as et,Qt as tt,R as nt,Ri as rt,Rn as it,Rr as at,Rt as ot,S as st,Si as ct,Sn as lt,Sr as ut,St as dt,T as ft,Ti as pt,Tn as mt,Tr as ht,Tt as gt,U as _t,Ui as vt,Un as yt,Ur as bt,Ut as xt,V as St,Vi as Ct,Vn as wt,Vr as Tt,Vt as Et,W as Dt,Wi as Ot,Wn as kt,Wr as At,Wt as jt,X as Mt,Xi as Nt,Xn as Pt,Xt as Ft,Y as It,Yi as Lt,Yn as Rt,Yr as zt,Yt as Bt,Z as Vt,Zi as Ht,Zn as Ut,Zt as Wt,_a as F,_i as Gt,_n as Kt,_r as qt,_t as Jt,aa as Yt,ai as Xt,an as Zt,ar as Qt,at as $t,b as en,ba as tn,bi as nn,bn as rn,bt as an,ca as on,ci as sn,cn,ct as ln,d as un,da as dn,di as fn,dn as pn,dt as mn,ea as hn,en as gn,er as _n,et as vn,f as yn,fa as I,fi as L,fn as bn,fr as xn,ft as Sn,ga as R,gn as Cn,gt as wn,h as Tn,ha as En,hi as Dn,hn as On,hr as kn,ht as An,i as jn,ia as Mn,ii as z,in as Nn,ir as Pn,it as Fn,j as In,ji as Ln,jn as Rn,jr as zn,jt as Bn,k as Vn,kn as Hn,kr as Un,kt as Wn,la as Gn,ln as Kn,lr as B,lt as qn,m as Jn,ma as Yn,mn as Xn,mt as Zn,n as Qn,na as $n,nn as er,nr as tr,nt as nr,oa as rr,on as ir,or as ar,ot as or,p as sr,pa as V,pi as cr,pn as lr,pr as ur,pt as dr,q as fr,qi as pr,qn as mr,qr as hr,qt as gr,r as _r,ra as vr,rn as yr,rr as br,rt as xr,s as Sr,sa as Cr,si as wr,sn as Tr,st as Er,t as Dr,ta as Or,tn as kr,tr as Ar,tt as jr,ua as Mr,ui as H,un as Nr,ut as Pr,v as Fr,va as Ir,vn as Lr,vr as Rr,vt as zr,w as Br,wi as Vr,wn as Hr,wr as U,wt as Ur,x as Wr,xi as Gr,xn as Kr,xt as qr,y as Jr,ya as Yr,yn as Xr,yr as Zr,yt as Qr,z as $r,zi as ei,zn as ti,zr as ni,zt as ri}from"./json-4.4.0-rc.1.js";import{c as ii}from"./fs-4.4.0-rc.1.js";var ai=1e4,oi=2e5,si=1e3,ci=ai;function li(){return ci}function ui(e){Number.isFinite(e)&&(ci=Math.min(oi,Math.max(si,Math.round(e))))}var di=class extends Error{range;constructor(e,t){super(e),this.range=t}};function W(e,t){return new di(e,t)}function fi(e){let t=e.split(`@`,2);return t.length===2&&t[0]===``&&/^\w/.test(t[1])}var pi=new Map;function mi(e,t){for(;e.length>0;){let n=e.shift();if(n===void 0)throw new H(`Docstring.parseAllTags`,`Atomicity violation: doc lines changed while parsing?`);let{line:r,range:i}=n;if(!fi(r))throw W(`Expected only tags at the end of docstring, but encountered a non-tag: ${r}`,i);let[a,...o]=r.split(` `);t.push(gi(a,o.join(` `),i))}}function hi(e,t){pi.set(e,t)}function gi(e,t,n){let r=pi.get(e);if(r===void 0)throw W(`Unknown doc tag: ${e}`,n);return r(t,n)}function _i(e){return Array.isArray(e.contents)&&e.contents.every(e=>typeof e==`string`)}hi(`@category`,(e,t)=>{let n=e.split(`,`).map(e=>e.trim()).filter(e=>e.length>0);if(n.length===0)throw W(`Error in @category tag: expected at least one category`,t);return{tag:`@category`,contents:n,range:t}});var vi=(e,t)=>{let n=t.loadModule(e.name,e.kind,e.alias);return n.tag===`trace`&&t.advanceStmt(),n},yi=(e,t)=>(t.addExports(e.names),t.advanceStmt(),G),bi=(e,t)=>{if(!t.isProcessingBlk)return t.beginProcessingBlk(e.expr),G;if(t.hasFramesRemaining())return t.stepFrame();if(ct(t.lastResult)&&(t.lastResult.name===void 0||t.lastResult.name===`##anonymous##`)){t.lastResult.name=e.name;let n=t.lastResult.contractTarget;ct(n)&&(n.name===void 0||n.name===`##anonymous##`)&&(n.name=e.name)}return t.topLevelEnv=t.topLevelEnv.extendWithTopLevel([e.name,t.lastResult]),t.advanceStmt(),G},xi=(e,t)=>t.isProcessingBlk?t.hasFramesRemaining()?t.stepFrame():(t.advanceStmt(),Ri):(t.beginProcessingBlk(e.expr),G),Si=(e,t)=>t.isProcessingBlk?t.hasFramesRemaining()?t.stepFrame():(t.advanceStmt(),G):(t.beginProcessingBlk(e.expr),G),Ci=class{name;env;values;ops;callRange;origin;home;hidden;constructor(e,t,n,r=Yr.none,i=`user`,a){this.name=e,this.env=t,this.values=[],this.ops=n.toReversed(),this.callRange=r,this.origin=i,this.home=a,this.hidden=i!==`user`}isFinished(){return this.ops.length===0}canTailCall(){return this.ops.every(e=>e.tag===`pop-scope`)}pushBlk(e){this.ops.push(...e.toReversed())}popInstr(){let e=this.ops.pop();if(!e)throw new H(`Frame.popInstr`,`Attempted to pop operation off frame ${this.name} when none remain`);return e}},wi=(e,t)=>(t.values.push(e.value),zi),Ti=(e,t)=>{if(!t.env.has(e.name))throw new L(`Runtime`,`Variable not found: ${e.name}`);let n=t.env.get(e.name);return t.origin===`builtin`&&ct(n)&&n.contractTarget!==void 0&&!t.env.isLocal(e.name)?(t.values.push(n.contractTarget),zi):(t.values.push(n),zi)},Ei=(e,t)=>(t.values.push(A(e.params,e.body,t.env.getScopes(),()=>{throw new H(`Fiber.ClsHandler`,`Closure.call was deprecated!`)},e.name,e.restParam,t.origin,t.home)),zi);function Di(e,t,n,r,i){if(Vr(e))try{return n.values.push(e(...t)),G}catch(t){if(t instanceof cr)return r.setMaxCallStackDepth(t.depth),n.values.push(void 0),G;let a=!n.name.startsWith(`##`),o=a?n.callRange:i;throw t instanceof Dn?((a||n.origin===`user`)&&(t.range??=o),t):t instanceof L?(t.range??=o,t.source??=a?n.name:e.name,t):new L(`Runtime`,`Unexpected error in Javascript function call: ${t instanceof Error?t.toString():String(t)}`,void 0,o,void 0)}if(ct(e)){if(t.length<e.params.length||!e.restParam&&t.length!==e.params.length)throw new L(`Runtime`,`Arity mismatch in function call: expected ${e.params.length.toString()} arguments, got ${t.length.toString()}`,void 0,i,void 0);let a=t.slice(0,e.params.length),o=e.params.map((e,t)=>[e,a[t]]).concat(e.restParam?[[e.restParam,V(t.slice(e.params.length))]]:[]),s=new Map(o),c=new Ci(e.name??`##anonymous##`,(e.home??r.topLevelEnv).withLocalScopes([...e.locals,s]),e.code,n.origin===`builtin`?n.callRange:i,e.origin??`user`,e.home);return n.canTailCall()?r.replaceFrame(c):r.pushFrame(c),G}throw new L(`Runtime`,`Not a function or closure: ${JSON.stringify(e)}`,void 0,i,void 0)}var Oi=(e,t,n)=>{if(t.values.length<e.numArgs+1)throw new H(`Fiber.ApHandler`,`Not enough values for application: expected ${(e.numArgs+1).toString()}, currently have ${t.values.length.toString()}`);let r=t.values.splice(-(e.numArgs+1)),i=r[0];return Di(i,e.numArgs===0?[]:r.splice(-e.numArgs),t,n,e.range)},ki=(e,t,n)=>{if(t.values.length<2)throw new H(`Fiber.ApSpreadHandler`,`Not enough values for ap-spread: expected 2, currently have ${t.values.length.toString()}`);let[r,i]=t.values.splice(-2);if(!pt(i))throw new L(`Runtime`,`expected a list, received ${I(i)}`,void 0,e.range,`apply`);return Di(r,Ln(i),t,n,e.range)},Ai=(e,t)=>{if(t.values.length===0)throw new H(`Fiber.MatchHandler`,`Match requires at least one value`);let n=t.values.pop(),r=e.branches.at(e.idx);if(!r)throw new L(`Runtime`,`Inexhaustive pattern match failure`,void 0,t.origin===`builtin`?t.callRange:e.range,void 0);let[i,a]=r,o=Cr(n,i);return o?(t.env=t.env.pushScope(o),t.pushBlk(a)):(t.pushBlk([Te(e.branches,e.range,e.idx+1)]),t.values.push(n)),G},ji=(e,t)=>{let n=e.bindings.length;if(e.idx===0)t.env=t.env.declareScope(e.bindings.flatMap(e=>on(e.pat)));else{if(t.values.length===0)throw new H(`Fiber.LetHandler`,`let binding value missing from the stack`);let n=t.values.pop(),r=e.bindings[e.idx-1],i=Cr(n,r.pat);if(!i)throw new L(`Runtime`,r.failMsg??`let: value did not match its pattern`,void 0,t.origin===`builtin`?t.callRange:r.pat.range,void 0);for(let[e,n]of i)t.env.assign(e,n)}return e.idx<n?t.pushBlk([...e.bindings[e.idx].value,Ot(e.bindings,e.body,e.range,e.idx+1,e.provenance)]):t.pushBlk(e.body),G},Mi=(e,t)=>{if(t.values.length===0)throw new H(`Fiber.IfHandler`,`if requires a guard value`);let n=t.values.pop();if(n===!0)t.pushBlk(e.thenB);else if(n===!1)t.pushBlk(e.elseB);else throw new L(`Runtime`,`if: expected a boolean guard, received ${I(n)}`);return G},Ni=(e,t)=>{throw new L(`Runtime`,`Hole encountered in program!`,void 0,t.origin===`builtin`?t.callRange:e.range)},Pi=(e,t)=>(t.env=t.env.popScope(),zi),Fi=(e,t,n)=>{if(t.values.length<2)throw new H(`Fiber.PushHandlerHandler`,`Expected a handler and a guarded function on the stack`);return n.handlerStack.push({frameDepth:n.frames.length,baseDepth:t.values.length-2,handler:t.values[t.values.length-2]}),zi},Ii=(e,t,n)=>{if(t.values.length<2)throw new H(`Fiber.PopHandlerHandler`,`Expected a handler and a result on the stack`);let r=Gn(t.values,`the frame's value stack`);return t.values.pop(),t.values.push(r),n.handlerStack.pop(),zi},Li=new Map,Ri={tag:`display`},G={tag:`trace`},zi={tag:`minor`};function Bi(e,t){return{tag:`import-file`,filename:e,...t===void 0?{}:{alias:t}}}function Vi(e,t){return{tag:`block-on`,action:e,range:t}}var Hi=class{topLevelEnv;frames=[];lastResult=null;handlerStack=[];prog;currStmtIdx=0;_isProcessingBlk=!1;_maxCallStackDepth=li();_ownCallStackDepth=!1;_exportedNames=new Set;closureOrigin;constructor(e,t=Yn.empty,n=`user`){this.prog=e,this.topLevelEnv=t,this.closureOrigin=n}step(){let e=this.prog.at(this.currStmtIdx);if(!e)throw new H(`Fiber.step`,`Attempted to step but no statements remain!`);switch(e.tag){case`import`:return vi(e,this);case`define`:return bi(e,this);case`export`:return yi(e,this);case`disp`:return xi(e,this);case`stmtexp`:return Si(e,this)}}addExports(e){for(let t of e)this._exportedNames.add(t)}getModule(){let e=this.topLevelEnv.getTopLevelAsModule(this._exportedNames);return e.allBindings=this.topLevelEnv.getTopLevelAsModule().bindings,e}advanceStmt(){this.frames=[],this.currStmtIdx++,this._isProcessingBlk=!1}get isProcessingBlk(){return this._isProcessingBlk}isDone(){return this.currStmtIdx>=this.prog.length}get stmtIndex(){return this.currStmtIdx}get statementCount(){return this.prog.length}statementAt(e){return this.prog.at(e)}get lastStatement(){let e=this.prog.at(this.currStmtIdx-1);if(!e)throw new H(`Fiber.lastStatement`,`Attempted to get the last completed statement in fiber when none exist at index ${(this.currStmtIdx-1).toString()}`);return e}beginProcessingBlk(e){this._isProcessingBlk=!0,this.pushFrame(new Ci(`##stmt-${this.currStmtIdx.toString()}##`,this.topLevelEnv,e,void 0,this.closureOrigin))}get currentFrame(){return this.frames.at(-1)}get maxCallStackDepth(){return this._maxCallStackDepth}get hasOwnCallStackDepth(){return this._ownCallStackDepth}setMaxCallStackDepth(e){this._maxCallStackDepth=e,this._ownCallStackDepth=!0}pushFrame(e){if(this.frames.length>=this._maxCallStackDepth)throw new L(`Runtime`,`Max call stack depth ${this._maxCallStackDepth.toString()} exceeded!`);this.frames.push(e)}popFrame(){this.frames.pop()}replaceFrame(e){e.hidden||=this.currentFrame?.hidden??!1,this.popFrame(),this.pushFrame(e)}hasFramesRemaining(){return this.frames.length>0}completeCurrentFrame(){let e=this.currentFrame;if(!e)throw new H(`Fiber.completeCurrentFrame`,`Attempted to complete a frame when none remain`);if(e.values.length!==1)throw new H(`Fiber.stepFrame`,`Frame must finish with exactly one value on the stack, finished with ${e.values.length.toString()} instead`);let t=e.values.pop();this.popFrame(),this.hasFramesRemaining()?this.currentFrame.values.push(t):this.lastResult=t}handleError(e){let t=this.handlerStack.pop();if(t===void 0)return!1;this.frames.length=t.frameDepth;let n=this.currentFrame;if(n===void 0)throw new H(`Fiber.handleError`,`Handler unwound past the bottom of the frame stack`);let r=0,i=n.popInstr();for(;i.tag!==`pop-handler`||r>0;)i.tag===`push-handler`?r++:i.tag===`pop-handler`&&r--,i=n.popInstr();return n.values.length=t.baseDepth,Di(t.handler,[e.message],n,this,e.range??Yr.none),!0}resumeWithValue(e){let t=this.currentFrame;if(t===void 0)throw new H(`Fiber.resumeWithValue`,`Attempted to resume a fiber with no current frame`);t.values.push(e),t.isFinished()&&this.completeCurrentFrame()}stepFrame(){if(!this.currentFrame)throw new L(`Runtime`,`Attempted to step stack frame when none exist!`);let e=this.currentFrame.popInstr(),t;switch(e.tag){case`lit`:t=wi(e,this.currentFrame,this);break;case`var`:t=Ti(e,this.currentFrame,this);break;case`cls`:t=Ei(e,this.currentFrame,this);break;case`ap`:t=Oi(e,this.currentFrame,this);break;case`match`:t=Ai(e,this.currentFrame,this);break;case`let`:t=ji(e,this.currentFrame,this);break;case`if`:t=Mi(e,this.currentFrame,this);break;case`hole`:t=Ni(e,this.currentFrame,this);break;case`pop-scope`:t=Pi(e,this.currentFrame,this);break;case`ap-spread`:t=ki(e,this.currentFrame,this);break;case`push-handler`:t=Fi(e,this.currentFrame,this);break;case`pop-handler`:t=Ii(e,this.currentFrame,this)}return this.currentFrame.isFinished()&&this.completeCurrentFrame(),t}loadModule(e,t,n){if(t===`builtin`){let t=Li.get(e);if(!t)throw new L(`Runtime`,`No such built-in library: ${e}`);return this.topLevelEnv=n===void 0?this.topLevelEnv.extendWithImport(e,t):this.topLevelEnv.extendWithQualifiedImport(n,t),G}return Bi(e,n)}},Ui=globalThis.scheduler;function Wi(){return Ui&&typeof Ui.yield==`function`?Ui.yield():new Promise(e=>{let t=new MessageChannel;t.port1.onmessage=()=>{t.port1.close(),t.port2.close(),e()},t.port2.postMessage(void 0)})}function Gi(e,t){return vr(`trace-start`,[`preamble`,`output`],[e,t])}sn.registerCustomRenderer(e=>c(e,`trace-start`),e=>{let t=e,n=t.preamble===``?0:t.preamble.length+1,r=t.output?sn.render(t.output,n):``;return[t.preamble,r].filter(e=>e!==``).join(` `)});function Ki(e){return vr(`trace-output`,[`output`],[e])}var qi=`--> `;sn.registerCustomRenderer(e=>c(e,`trace-output`),e=>`${qi}${sn.render(e.output,4)}`);function Ji(e){switch(e.tag){case`pwild`:return Ht(e.range);case`id`:return Lt(e.name,e.range);case`plit`:return xe(e.value,e.range);case`pctor`:return pr(e.name.name,e.args.map(Ji),e.range);case`pvec`:return Nt(e.args.map(Ji),e.range)}}function K(e){switch(e.tag){case`lit`:return[se(e.value,e.range,e.provenance)];case`id`:return[Mn(e.name,e.range)];case`hole`:return[Ct(e.range)];case`app`:return e.head.tag===`id`&&e.head.name===`##ap-spread##`?[...K(e.args[0]),...K(e.args[1]),Re(e.range)]:[...K(e.head),...e.args.flatMap(K),Pe(e.args.length,e.range,e.provenance)];case`lam`:return[ge(e.params.map(e=>e.name),K(e.body),`##anonymous##`,e.range,e.restParam?.name,e.provenance)];case`let`:{let t=e.bindings.map(e=>({pat:Ji(e.pat),value:K(e.value),failMsg:`let: value did not match pattern ${zt(e.pat)}`}));return[Ot(t,K(e.body),e.range,0,e.provenance),hn(e.range)]}case`if`:return[...K(e.guard),de(K(e.ifB),K(e.elseB),e.range,e.provenance)];case`match`:return[...K(e.scrutinee),Te(e.branches.map(({pat:e,body:t})=>[Ji(e),K(t)]),e.range),hn(e.range)];default:throw new H(`lowerExpr`,`Non-core expression encountered: ${e.tag}`)}}function Yi(e,t=!0){switch(e.tag){case`import`:return vt(e.module,e.kind,e.range,e.alias);case`export`:return p(e.names.map(e=>e.name),e.range);case`define`:return rt(e.name.name,K(e.value),e.range);case`display`:return ei(K(e.value),e.range);case`stmtexp`:return t?ei(K(e.expr),e.range):$n(K(e.expr),e.range);default:throw new H(`lowerStmt`,`Unknown expected statement type: ${e.tag}`)}}function Xi(e,t=!0){return e.map(e=>Yi(e,t))}function Zi(e,t){let n=e=>Zi(e,t);switch(e.tag){case`lit`:case`hole`:return e;case`id`:return e.name===`%&`?(t.hasRest=!0,e):e.name===`%`?(t.maxNum=Math.max(t.maxNum,1),N(`%1`,e.range)):(/^%[1-9][0-9]*$/.test(e.name)&&(t.maxNum=Math.max(t.maxNum,parseInt(e.name.slice(1),10))),e);case`app`:return U(n(e.head),e.args.map(n),e.range,e.provenance);case`lam`:return ie(e.params,n(e.body),e.range,e.restParam,e.provenance);case`let`:return ve(e.bindings.map(e=>({pat:e.pat,value:n(e.value)})),n(e.body),e.range,e.provenance);case`if`:return P(n(e.guard),n(e.ifB),n(e.elseB),e.range,e.provenance);case`match`:return at(n(e.scrutinee),e.branches.map(e=>({pat:e.pat,body:n(e.body)})),e.range);default:throw new H(`collectAndNormalizePercent`,`Unexpected form: ${e.tag}`)}}function q(e){switch(e.tag){case`id`:return e;case`lit`:return e;case`hole`:return e;case`app`:return U(q(e.head),e.args.map(q),e.range);case`lam`:return ie(e.params,q(e.body),e.range,e.restParam);case`let`:return ve(e.bindings.map(e=>({pat:e.pat,value:q(e.value)})),q(e.body),e.range);case`if`:return P(q(e.guard),q(e.ifB),q(e.elseB),e.range);case`match`:return at(q(e.scrutinee),e.branches.map(e=>({pat:e.pat,body:q(e.body)})),e.range);case`begin`:{let t=e.exps.map(q),n=t[t.length-1];for(let r=t.length-2;r>=0;r--)n=ve([{pat:At(e.range),value:t[r]}],n,e.range,`begin`);return n}case`and`:{let t=e.exps.map(q),n=M(!0,e.range,`and`);for(let r=t.length-1;r>=0;r--)n=P(t[r],n,M(!1,e.range,`and`),e.range,`and`);return n}case`or`:{let t=e.exps.map(q),n=M(!1,e.range,`or`);for(let r=t.length-1;r>=0;r--)n=P(t[r],M(!0,e.range,`or`),n,e.range,`or`);return n}case`cond`:{let t=e.branches.map(e=>({test:q(e.test),body:q(e.body)})),n=U(N(`##error##`,e.range),[M(`No matching clause in cond`,e.range)],e.range,`cond`);for(let r=t.length-1;r>=0;r--)n=P(t[r].test,t[r].body,n,e.range,`cond`);return n}case`anonfn`:{let t={maxNum:0,hasRest:!1},n=Zi(q(e.body),t),r=[];for(let n=1;n<=t.maxNum;n++)r.push(N(`%${String(n)}`,e.range));let i=t.hasRest?N(`%&`,e.range):void 0;return ie(r,n,e.range,i,`anon-fn`)}case`vec`:return U(N(`##mkVec##`,e.range),e.exps.map(q),e.range,`vector-lit`);case`obj`:return U(N(`##mkObj##`,e.range),e.pairs.flatMap(({key:e,value:t})=>[q(e),q(t)]),e.range,`obj-lit`)}}function Qi(e){switch(e.tag){case`import`:return[e];case`define`:return[T(e.name,q(e.value),e.range,e.docComments)];case`export`:return[e];case`defexport`:return[T(e.name,q(e.value),e.range,e.docComments,`define-export`),u([e.name],e.range,`define-export`)];case`display`:return[Un(q(e.value),e.range)];case`struct`:return[T(e.name,U(N(`##mkCtorFn##`),[M(e.name.name),M(e.fields.map(e=>e.name))],e.range),e.range),T(N(`${e.name.name}?`,e.range),U(N(`##mkPredFn##`),[M(e.name.name)],e.range),e.range),...e.fields.map(t=>T(N(`${e.name.name}-${t.name}`,e.range),U(N(`##mkGetFn##`),[M(e.name.name),M(t.name)]),e.range))];case`stmtexp`:return[ce(q(e.expr),e.range)]}}function $i(e){return e.flatMap(Qi)}function J(e,t,n,r){return{phase:e,severity:t,message:n,range:r}}function ea(e){let t=e.phase===`Docstring`?`Docstring`:`Parser`;return new L(t,e.message,e.modName,e.range,e.source)}var ta=/^[+-]?\d+$/,na=/^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/;function ra(e){return ta.test(e)?parseInt(e):parseFloat(e)}function ia(e,t){if(e.length===0)throw new H(`parseStringLiteral`,`Empty string literal (with no quote!)`);if(!e.startsWith(`"`))throw new L(`Parser`,`String literal must begin with a quote`,void 0,t);let n=``;for(let r=1;r<e.length;r++)if(e[r]===`"`)return n;else if(e[r]===`\\`){if(r+1>=e.length)throw new L(`Parser`,`Escape character "\\" cannot occur at the end of a string.`,void 0,t);let i=e[r+1];switch(i){case`a`:n+=`\x07`;break;case`b`:n+=`\b`;break;case`t`:n+=`	`;break;case`n`:n+=`
`;break;case`v`:n+=`\v`;break;case`f`:n+=`\f`;break;case`r`:n+=`\r`;break;case`e`:n+=`\x1B`;break;case`"`:n+=`"`;break;case`'`:n+=`'`;break;case`\\`:n+=`\\`;break;default:if(i>=`0`&&i<=`9`)throw new L(`Parser`,`Octal escape codes not supported`,void 0,t);if(i===`x`)throw new L(`Parser`,`Hex escape codes not supported`,void 0,t);if(i===`u`||i===`U`)throw new L(`Parser`,`Unicode escape codes not supported`,void 0,t);i===`
`||(n+=i)}r+=1}else n+=e[r];return n}function aa(e,t){let n=e.slice(2);if(n.length===1)return qe(n);if(rr.has(n))return qe(rr.get(n));throw new L(`Parser`,`Invalid character literal: ${e}`,void 0,t)}function oa(e){return!ta.test(e)&&!na.test(e)&&e!==`#t`&&e!==`#f`&&e!==`null`&&!e.startsWith(`"`)&&!e.startsWith(`#\\`)&&e.length>0}var sa=[`??`,`and`,`begin`,`cond`,`define`,`define-export`,`export`,`if`,`import`,`display`,`lambda`,`let`,`match`,`or`,`struct`];function ca(e,t){try{return e()}catch(e){if(t(e))throw e;return}}var la=Symbol(`##SCAMPER_TAGGED##`);function ua(e,t){return{[la]:e,value:t}}function da(e){return typeof e==`object`&&!!e&&la in e}function fa(e,t){return da(e)&&e[la]===t}function pa(e){let t=[];for(let n=0;n<e.length;n+=2)t.push([e[n],e[n+1]]);return t}var ma=e=>/\s/.test(e),ha={Beginning:`beginning`,PrePredicate:`pre-predicate`},ga=class extends di{constructor(e,t){super(`Error while parsing param in doc string: ${e}`,t)}},_a=class extends ga{loc;constructor(e,t,n){super(t,n),this.loc=e}},va=class extends ga{},ya=class extends ga{};function ba(e){let t=e.shift();if(t===void 0)throw new H(`Docstring.parseSingleParam`,`Doc lines expected to be not empty when calling parseSingleParam`);let n=ca(()=>Ca(t),Da);if(!n)return e.unshift(t),Ga.Description;let{param:r,beginningWhitespaces:i}=n,a=e.shift();if(a===void 0)throw W(`Doc string is missing function description`,t.range);if(r.description=ca(()=>Ta(a,i),Da),r.description===void 0)return e.unshift(a),r;let o=a.range;for(;e.length>0;){let t=e.shift();if(t===void 0)throw W(`Doc string is missing function description`,a.range);o=t.range;let n=ca(()=>Ta(t,i),Da);if(n===void 0)return e.unshift(t),r;r.description+=` `+n}throw W(`Doc string is missing function description`,o)}var xa=1,Sa=1;function Ca(e){let t=Ea(e,xa,ha.Beginning),{line:n,range:r}=e,i=n.slice(t).split(`:`);if(i.length<2)throw new va(`Line is missing separating colon between name and predicate`,r);let[a,...o]=i,s=o.join(`:`),c=a.trimEnd(),l=[];oa(c)?c.startsWith(`_`)?l.push(`Identifiers cannot begin with "_" unless inside of patterns`):sa.includes(c)&&l.push(`The identifier "${c}" is a reserved word and cannot be used as a variable name`):l.push(`Expected an identifier`);let u=l.length>0?`<error>`:c;if(l.length>0)throw new ya(`Name field is malformed, ${l[0]}`,r);let d=Ea({line:s,range:r},Sa,ha.PrePredicate),{program:f,diagnostics:p}=Fc(s.slice(d).trim());if(!f||p.length>0||f.length>1)throw new ya(`Predicate field is malformed${p.length>0?`, `+p[0].message:``}`,r);if(f.length<1)throw new va(`Predicate field is missing`,r);let m=f[0];if(!Rr(m))throw new ya(`Predicate should be an expression`,r);if(!Ua(m.expr))throw new ya(`Predicate should be either a simple predicate identifier or a complex predicate application`,r);let h=m.expr;return h.range=r,{param:{name:u,predicate:h,range:r},beginningWhitespaces:t}}var wa=1;function Ta(e,t){let n=Ea(e,t+wa,ha.Beginning);return e.line.slice(n).trim()}function Ea({line:e,range:t},n,r){let i=0;for(let a of e){if(ma(a)){i++;continue}if(i<n)throw new _a(r,`Line does not have enough ${r} whitespace, expected at least ${n.toString()} but got ${i.toString()}`,t);break}return i}function Da(e){return!(e instanceof _a)||e.loc!==ha.Beginning}function Oa(e){let t=``;for(;e.length>0;){let n=e.shift();if(n===void 0)throw new H(`Docstring.parseFunctionDescription`,`Atomicity violation: doc lines changed while parsing?`);if(n.line.startsWith(`@`))return e.unshift(n),{stage:Ga.Tags,description:t.trim()};t+=n.line+` `}return t.trim()}function ka(e){return typeof e.contents==`object`&&e.contents!==null&&`functionCall`in e.contents&&`result`in e.contents}var Aa=` -> `;function ja(e,t,n){throw W(`Error in @example tag: ${e}${n?`, ${n}`:``}`,t)}function Ma(e,t,n){let{program:r,diagnostics:i}=Fc(e.trim());(!r||i.length>0)&&ja(`${n} is malformed`,t,i[0]?.message),r.length<1&&ja(`${n} is missing`,t),r.length>1&&ja(`more than one expression found in ${n}`,t);let a=r[0];return a.range=t,a}hi(`@example`,(e,t)=>{let n=e.split(Aa);n.length<2&&ja(`missing separator, expected "expression -> result"`,t);let r=n[0],i=n.slice(1).join(Aa),a=Ma(r,t,`function call`);Rr(a)||ja(`function call should be an expression`,t);let o=a.expr;ur(o)||ja(`function call should be an application expression`,t);let s=Ma(i,t,`result`);return Rr(s)||ja(`result should be an expression`,t),{tag:`@example`,contents:{functionCall:o,result:s.expr},range:t}});var Na=[`null`];function Pa(e,t,n=!1){if(!oa(e)&&!(n&&Na.includes(e)))throw W(`Expected an identifier`,t);if(e.startsWith(`_`))throw W(`Identifiers cannot begin with "_" unless inside of patterns`,t);if(sa.includes(e))throw W(`The identifier "${e}" is a reserved word and cannot be used as a variable name`,t)}function Fa({line:e,range:t}){if(e.startsWith(` `))throw W(`Function signature cannot start with whitespace`,t);if(!e.startsWith(`(`)||!e.endsWith(`)`))throw W(`Malformed function signature`,t);let n=e.slice(1,-1).trim().split(/\s+/).filter(e=>e.length>0);if(n.length===0)throw W(`Function signature is missing`,t);let[r,...i]=n;Pa(r,t);let a=i.indexOf(`&`),o,s;if(a===-1)o=i;else{if(a!==i.length-2)throw W(`Malformed rest parameter: expected a single "&" immediately before the final (rest) parameter name`,t);o=i.slice(0,a),s=i[a+1]}if(i.filter(e=>e===`&`).length>1)throw W(`Malformed function signature: more than one "&" found`,t);let c=o.findIndex(Ia),l=c===-1?[]:o.slice(c),u=c===-1?o:o.slice(0,c);if(!l.every(Ia))throw W(`Malformed function signature: a required parameter cannot follow an optional one`,t);let d=e=>e.map(e=>(La(e,t),Pa(e,t),N(e,t))),f=d(u),p=d(l.map(e=>e.slice(1,-1))),m;return s!==void 0&&(La(s,t),Pa(s,t),m=N(s,t)),{tag:`app`,head:N(r,t),args:f,optArgs:p,restParam:m,range:t}}function Ia(e){return e.length>2&&e.startsWith(`[`)&&e.endsWith(`]`)}function La(e,t){if(e===`&`||e.includes(`[`)||e.includes(`]`))throw W(`Malformed parameter "${e}": an optional parameter is written as a name in one pair of brackets, e.g. "[end]"`,t)}function Ra({line:e,range:t}){let{program:n,diagnostics:r}=Fc(e);if(!n||r.length>0||n.length>1)throw W(`Malformed predicate field`,t);if(n.length<1)throw W(`Predicate field is missing`,t);let i=n[0];if(!Rr(i))throw W(`Not a contract signature`,t);if(!Ua(i.expr))throw W(`Not a contract signature. Expected a variable or variable application`,t);let a=i.expr;return a.range=t,a}function za({line:e,range:t}){return e.trimStart().startsWith(`(`)?Ba(e,t):Va(e,t)}function Ba(e,t){let n=` -> `,[r,...i]=e.split(n);if(e.split(n).length<2)throw W(`Missing separator in doc string signature`,t);return{function:Fa({line:r,range:t}),predicate:Ra({line:i.join(n),range:t}),range:t,isConstant:!1}}function Va(e,t){let n=e.indexOf(`:`);if(n===-1)throw W(`Missing separator in doc string signature: expected "(name ...) -> predicate" or "name: predicate"`,t);let r=e.slice(0,n).trim();if(r.includes(` `))throw W(`A constant signature names one binding and takes no parameters`,t);Pa(r,t,!0);let i=Ra({line:e.slice(n+1),range:t});return{function:{tag:`app`,head:N(r,t),args:[],optArgs:[],range:t},predicate:i,range:t,isConstant:!0}}function Ha(e){return Zr(e.head)&&e.args.every(e=>Zr(e)||ur(e)&&Ha(e))}function Ua(e){return Zr(e)||ur(e)&&Ha(e)}var Wa=Symbol(`ParseStageTag`),Ga={Params:ua(Wa,`params`),Description:ua(Wa,`description`),Tags:ua(Wa,`tags`)};function Ka(e){let t=e.shift();if(t===void 0)throw new H(`Docstring.parseDocString`,`Attempted to parse docstring from comment block with no doc lines!`);let n=t.range,r=(e.at(-1)??t).range,i=za(t),a=[],o=[],s=new Set(i.function.optArgs.map(e=>e.name)),c,l=``,u=[],d=Ga.Params;for(;e.length>0;)switch(d){case Ga.Params:{let t=ba(e);if(fa(t,Wa)){let e=i.function.args.length+i.function.optArgs.length+ +!!i.function.restParam;if(a.length+o.length+ +!!c!==e)throw W(i.function.restParam&&!c?`Rest parameter "${i.function.restParam.name}" was declared in the signature but not documented`:`Encountered function description before all parameters were described`,n);qa(a,i.function.args,n),qa(o,i.function.optArgs,n),d=t}else t.name===i.function.restParam?.name?c=t:s.has(t.name)?o.push(t):a.push(t);break}case Ga.Description:{let t=Oa(e);typeof t==`object`&&`stage`in t?{stage:d,description:l}=t:l=t;break}case Ga.Tags:mi(e,u)}if(l===``)throw W(`Docstring must have a function description`,r);return{signature:i,params:a,optParams:o,restParam:c,description:l,tags:u,range:new Yr(n.begin,r.end)}}function qa(e,t,n){let r=e.findIndex((e,n)=>n>=t.length||e.name!==t[n].name);if(r===-1)return;let i=e[r].name;throw W(r>=t.length?`Parameter "${i}" is not declared in the signature`:`Parameter "${i}" does not match the signature, which declares "${t[r].name}" in that position`,n)}var Ja=`;;; `;function Ya({line:e,range:t}){let[n,...r]=e.split(Ja);if(r.length!==0&&n===``)return{line:r.join(Ja),range:t}}function Xa(e){return e.map(Ya).filter(e=>e!==void 0)}function Za(e){let t=Xa(e);if(t.length===0)return{diagnostics:[]};try{return{doc:Ka(t),diagnostics:[]}}catch(e){if(e instanceof di)return{diagnostics:[J(`Docstring`,`warning`,e.message,e.range)]};throw e}}var Qa=1024,$a=0,eo=class{constructor(e,t){this.from=e,this.to=t}},Y=class{constructor(e={}){this.id=$a++,this.perNode=!!e.perNode,this.deserialize=e.deserialize||(()=>{throw Error(`This node type doesn't define a deserialize function`)}),this.combine=e.combine||null}add(e){if(this.perNode)throw RangeError(`Can't add per-node props to node types`);return typeof e!=`function`&&(e=ro.match(e)),t=>{let n=e(t);return n===void 0?null:[this,n]}}};Y.closedBy=new Y({deserialize:e=>e.split(` `)}),Y.openedBy=new Y({deserialize:e=>e.split(` `)}),Y.group=new Y({deserialize:e=>e.split(` `)}),Y.isolate=new Y({deserialize:e=>{if(e&&e!=`rtl`&&e!=`ltr`&&e!=`auto`)throw RangeError(`Invalid value for isolate: `+e);return e||`auto`}}),Y.contextHash=new Y({perNode:!0}),Y.lookAhead=new Y({perNode:!0}),Y.mounted=new Y({perNode:!0});var to=class{constructor(e,t,n,r=!1){this.tree=e,this.overlay=t,this.parser=n,this.bracketed=r}static get(e){return e&&e.props&&e.props[Y.mounted.id]}},no=Object.create(null),ro=class e{constructor(e,t,n,r=0){this.name=e,this.props=t,this.id=n,this.flags=r}static define(t){let n=t.props&&t.props.length?Object.create(null):no,r=+!!t.top|(t.skipped?2:0)|(t.error?4:0)|(t.name==null?8:0),i=new e(t.name||``,n,t.id,r);if(t.props){for(let e of t.props)if(Array.isArray(e)||(e=e(i)),e){if(e[0].perNode)throw RangeError(`Can't store a per-node prop on a node type`);n[e[0].id]=e[1]}}return i}prop(e){return this.props[e.id]}get isTop(){return(this.flags&1)>0}get isSkipped(){return(this.flags&2)>0}get isError(){return(this.flags&4)>0}get isAnonymous(){return(this.flags&8)>0}is(e){if(typeof e==`string`){if(this.name==e)return!0;let t=this.prop(Y.group);return t?t.indexOf(e)>-1:!1}return this.id==e}static match(e){let t=Object.create(null);for(let n in e)for(let r of n.split(` `))t[r]=e[n];return e=>{for(let n=e.prop(Y.group),r=-1;r<(n?n.length:0);r++){let i=t[r<0?e.name:n[r]];if(i)return i}}}};ro.none=new ro(``,Object.create(null),0,8);var io=class e{constructor(e){this.types=e;for(let t=0;t<e.length;t++)if(e[t].id!=t)throw RangeError(`Node type ids should correspond to array positions when creating a node set`)}extend(...t){let n=[];for(let e of this.types){let r=null;for(let n of t){let t=n(e);if(t){r||=Object.assign({},e.props);let n=t[1],i=t[0];i.combine&&i.id in r&&(n=i.combine(r[i.id],n)),r[i.id]=n}}n.push(r?new ro(e.name,r,e.id,e.flags):e)}return new e(n)}},ao=new WeakMap,oo=new WeakMap,X;(function(e){e[e.ExcludeBuffers=1]=`ExcludeBuffers`,e[e.IncludeAnonymous=2]=`IncludeAnonymous`,e[e.IgnoreMounts=4]=`IgnoreMounts`,e[e.IgnoreOverlays=8]=`IgnoreOverlays`,e[e.EnterBracketed=16]=`EnterBracketed`})(X||={});var Z=class e{constructor(e,t,n,r,i){if(this.type=e,this.children=t,this.positions=n,this.length=r,this.props=null,i&&i.length){this.props=Object.create(null);for(let[e,t]of i)this.props[typeof e==`number`?e:e.id]=t}}toString(){let e=to.get(this);if(e&&!e.overlay)return e.tree.toString();let t=``;for(let e of this.children){let n=e.toString();n&&(t&&(t+=`,`),t+=n)}return this.type.name?(/\W/.test(this.type.name)&&!this.type.isError?JSON.stringify(this.type.name):this.type.name)+(t.length?`(`+t+`)`:``):t}cursor(e=0){return new xo(this.topNode,e)}cursorAt(e,t=0,n=0){let r=new xo(ao.get(this)||this.topNode);return r.moveTo(e,t),ao.set(this,r._tree),r}get topNode(){return new po(this,0,0,null)}resolve(e,t=0){let n=uo(ao.get(this)||this.topNode,e,t,!1);return ao.set(this,n),n}resolveInner(e,t=0){let n=uo(oo.get(this)||this.topNode,e,t,!0);return oo.set(this,n),n}resolveStack(e,t=0){return bo(this,e,t)}iterate(e){let{enter:t,leave:n,from:r=0,to:i=this.length}=e,a=e.mode||0,o=(a&X.IncludeAnonymous)>0;for(let e=this.cursor(a|X.IncludeAnonymous);;){let a=!1;if(e.from<=i&&e.to>=r&&(!o&&e.type.isAnonymous||t(e)!==!1)){if(e.firstChild())continue;a=!0}for(;a&&n&&(o||!e.type.isAnonymous)&&n(e),!e.nextSibling();){if(!e.parent())return;a=!0}}}prop(e){return e.perNode?this.props?this.props[e.id]:void 0:this.type.prop(e)}get propValues(){let e=[];if(this.props)for(let t in this.props)e.push([+t,this.props[t]]);return e}balance(t={}){return this.children.length<=8?this:Eo(ro.none,this.children,this.positions,0,this.children.length,0,this.length,(t,n,r)=>new e(this.type,t,n,r,this.propValues),t.makeTree||((t,n,r)=>new e(ro.none,t,n,r)))}static build(e){return Co(e)}};Z.empty=new Z(ro.none,[],[],0);var so=class e{constructor(e,t){this.buffer=e,this.index=t}get id(){return this.buffer[this.index-4]}get start(){return this.buffer[this.index-3]}get end(){return this.buffer[this.index-2]}get size(){return this.buffer[this.index-1]}get pos(){return this.index}next(){this.index-=4}fork(){return new e(this.buffer,this.index)}},co=class e{constructor(e,t,n){this.buffer=e,this.length=t,this.set=n}get type(){return ro.none}toString(){let e=[];for(let t=0;t<this.buffer.length;)e.push(this.childString(t)),t=this.buffer[t+3];return e.join(`,`)}childString(e){let t=this.buffer[e],n=this.buffer[e+3],r=this.set.types[t],i=r.name;if(/\W/.test(i)&&!r.isError&&(i=JSON.stringify(i)),e+=4,n==e)return i;let a=[];for(;e<n;)a.push(this.childString(e)),e=this.buffer[e+3];return i+`(`+a.join(`,`)+`)`}findChild(e,t,n,r,i){let{buffer:a}=this,o=-1;for(let s=e;s!=t&&!(lo(i,r,a[s+1],a[s+2])&&(o=s,n>0));s=a[s+3]);return o}slice(t,n,r){let i=this.buffer,a=new Uint16Array(n-t),o=0;for(let e=t,s=0;e<n;){a[s++]=i[e++],a[s++]=i[e++]-r;let n=a[s++]=i[e++]-r;a[s++]=i[e++]-t,o=Math.max(o,n)}return new e(a,o,this.set)}};function lo(e,t,n,r){switch(e){case-2:return n<t;case-1:return r>=t&&n<t;case 0:return n<t&&r>t;case 1:return n<=t&&r>t;case 2:return r>t;case 4:return!0}}function uo(e,t,n,r){for(;e.from==e.to||(n<1?e.from>=t:e.from>t)||(n>-1?e.to<=t:e.to<t);){let t=!r&&e instanceof po&&e.index<0?null:e.parent;if(!t)return e;e=t}let i=r?0:X.IgnoreOverlays;if(r)for(let r=e,a=r.parent;a;r=a,a=r.parent)r instanceof po&&r.index<0&&a.enter(t,n,i)?.from!=r.from&&(e=a);for(;;){let r=e.enter(t,n,i);if(!r)return e;e=r}}var fo=class{cursor(e=0){return new xo(this,e)}getChild(e,t=null,n=null){let r=mo(this,e,t,n);return r.length?r[0]:null}getChildren(e,t=null,n=null){return mo(this,e,t,n)}resolve(e,t=0){return uo(this,e,t,!1)}resolveInner(e,t=0){return uo(this,e,t,!0)}matchContext(e){return ho(this.parent,e)}enterUnfinishedNodesBefore(e){let t=this.childBefore(e),n=this;for(;t;){let e=t.lastChild;if(!e||e.to!=t.to)break;e.type.isError&&e.from==e.to?(n=t,t=e.prevSibling):t=e}return n}get node(){return this}get next(){return this.parent}},po=class e extends fo{constructor(e,t,n,r){super(),this._tree=e,this.from=t,this.index=n,this._parent=r}get type(){return this._tree.type}get name(){return this._tree.type.name}get to(){return this.from+this._tree.length}nextChild(t,n,r,i,a=0){for(let o=this;;){for(let{children:s,positions:c}=o._tree,l=n>0?s.length:-1;t!=l;t+=n){let l=s[t],u=c[t]+o.from,d;if(a&X.EnterBracketed&&l instanceof Z&&(d=to.get(l))&&!d.overlay&&d.bracketed&&r>=u&&r<=u+l.length||lo(i,r,u,u+l.length)){if(l instanceof co){if(a&X.ExcludeBuffers)continue;let e=l.findChild(0,l.buffer.length,n,r-u,i);if(e>-1)return new _o(new go(o,l,t,u),null,e)}else if(a&X.IncludeAnonymous||!l.type.isAnonymous||So(l)){let s;if(!(a&X.IgnoreMounts)&&(s=to.get(l))&&!s.overlay)return new e(s.tree,u,t,o);let c=new e(l,u,t,o);return a&X.IncludeAnonymous||!c.type.isAnonymous?c:c.nextChild(n<0?l.children.length-1:0,n,r,i,a)}}}if(a&X.IncludeAnonymous||!o.type.isAnonymous||(t=o.index>=0?o.index+n:n<0?-1:o._parent._tree.children.length,o=o._parent,!o))return null}}get firstChild(){return this.nextChild(0,1,0,4)}get lastChild(){return this.nextChild(this._tree.children.length-1,-1,0,4)}childAfter(e){return this.nextChild(0,1,e,2)}childBefore(e){return this.nextChild(this._tree.children.length-1,-1,e,-2)}prop(e){return this._tree.prop(e)}enter(t,n,r=0){let i;if(!(r&X.IgnoreOverlays)&&(i=to.get(this._tree))&&i.overlay){let a=t-this.from,o=r&X.EnterBracketed&&i.bracketed;for(let{from:t,to:r}of i.overlay)if((n>0||o?t<=a:t<a)&&(n<0||o?r>=a:r>a))return new e(i.tree,i.overlay[0].from+this.from,-1,this)}return this.nextChild(0,1,t,n,r)}nextSignificantParent(){let e=this;for(;e.type.isAnonymous&&e._parent;)e=e._parent;return e}get parent(){return this._parent?this._parent.nextSignificantParent():null}get nextSibling(){return this._parent&&this.index>=0?this._parent.nextChild(this.index+1,1,0,4):null}get prevSibling(){return this._parent&&this.index>=0?this._parent.nextChild(this.index-1,-1,0,4):null}get tree(){return this._tree}toTree(){return this._tree}toString(){return this._tree.toString()}};function mo(e,t,n,r){let i=e.cursor(),a=[];if(!i.firstChild())return a;if(n!=null){for(let e=!1;!e;)if(e=i.type.is(n),!i.nextSibling())return a}for(;;){if(r!=null&&i.type.is(r))return a;if(i.type.is(t)&&a.push(i.node),!i.nextSibling())return r==null?a:[]}}function ho(e,t,n=t.length-1){for(let r=e;n>=0;r=r.parent){if(!r)return!1;if(!r.type.isAnonymous){if(t[n]&&t[n]!=r.name)return!1;n--}}return!0}var go=class{constructor(e,t,n,r){this.parent=e,this.buffer=t,this.index=n,this.start=r}},_o=class e extends fo{get name(){return this.type.name}get from(){return this.context.start+this.context.buffer.buffer[this.index+1]}get to(){return this.context.start+this.context.buffer.buffer[this.index+2]}constructor(e,t,n){super(),this.context=e,this._parent=t,this.index=n,this.type=e.buffer.set.types[e.buffer.buffer[n]]}child(t,n,r){let{buffer:i}=this.context,a=i.findChild(this.index+4,i.buffer[this.index+3],t,n-this.context.start,r);return a<0?null:new e(this.context,this,a)}get firstChild(){return this.child(1,0,4)}get lastChild(){return this.child(-1,0,4)}childAfter(e){return this.child(1,e,2)}childBefore(e){return this.child(-1,e,-2)}prop(e){return this.type.prop(e)}enter(t,n,r=0){if(r&X.ExcludeBuffers)return null;let{buffer:i}=this.context,a=i.findChild(this.index+4,i.buffer[this.index+3],n>0?1:-1,t-this.context.start,n);return a<0?null:new e(this.context,this,a)}get parent(){return this._parent||this.context.parent.nextSignificantParent()}externalSibling(e){return this._parent?null:this.context.parent.nextChild(this.context.index+e,e,0,4)}get nextSibling(){let{buffer:t}=this.context,n=t.buffer[this.index+3];return n<(this._parent?t.buffer[this._parent.index+3]:t.buffer.length)?new e(this.context,this._parent,n):this.externalSibling(1)}get prevSibling(){let{buffer:t}=this.context,n=this._parent?this._parent.index+4:0;return this.index==n?this.externalSibling(-1):new e(this.context,this._parent,t.findChild(n,this.index,-1,0,4))}get tree(){return null}toTree(){let e=[],t=[],{buffer:n}=this.context,r=this.index+4,i=n.buffer[this.index+3];if(i>r){let a=n.buffer[this.index+1];e.push(n.slice(r,i,a)),t.push(0)}return new Z(this.type,e,t,this.to-this.from)}toString(){return this.context.buffer.childString(this.index)}};function vo(e){if(!e.length)return null;let t=0,n=e[0];for(let r=1;r<e.length;r++){let i=e[r];(i.from>n.from||i.to<n.to)&&(n=i,t=r)}let r=n instanceof po&&n.index<0?null:n.parent,i=e.slice();return r?i[t]=r:i.splice(t,1),new yo(i,n)}var yo=class{constructor(e,t){this.heads=e,this.node=t}get next(){return vo(this.heads)}};function bo(e,t,n){let r=e.resolveInner(t,n),i=null;for(let e=r instanceof po?r:r.context.parent;e;e=e.parent)if(e.index<0){let a=e.parent;(i||=[r]).push(a.resolve(t,n)),e=a}else{let a=to.get(e.tree);if(a&&a.overlay&&a.overlay[0].from<=t&&a.overlay[a.overlay.length-1].to>=t){let o=new po(a.tree,a.overlay[0].from+e.from,-1,e);(i||=[r]).push(uo(o,t,n,!1))}}return i?vo(i):r}var xo=class{get name(){return this.type.name}constructor(e,t=0){if(this.buffer=null,this.stack=[],this.index=0,this.bufferNode=null,this.mode=t&~X.EnterBracketed,e instanceof po)this.yieldNode(e);else{this._tree=e.context.parent,this.buffer=e.context;for(let t=e._parent;t;t=t._parent)this.stack.unshift(t.index);this.bufferNode=e,this.yieldBuf(e.index)}}yieldNode(e){return e?(this._tree=e,this.type=e.type,this.from=e.from,this.to=e.to,!0):!1}yieldBuf(e,t){this.index=e;let{start:n,buffer:r}=this.buffer;return this.type=t||r.set.types[r.buffer[e]],this.from=n+r.buffer[e+1],this.to=n+r.buffer[e+2],!0}yield(e){return e?e instanceof po?(this.buffer=null,this.yieldNode(e)):(this.buffer=e.context,this.yieldBuf(e.index,e.type)):!1}toString(){return this.buffer?this.buffer.buffer.childString(this.index):this._tree.toString()}enterChild(e,t,n){if(!this.buffer)return this.yield(this._tree.nextChild(e<0?this._tree._tree.children.length-1:0,e,t,n,this.mode));let{buffer:r}=this.buffer,i=r.findChild(this.index+4,r.buffer[this.index+3],e,t-this.buffer.start,n);return i<0?!1:(this.stack.push(this.index),this.yieldBuf(i))}firstChild(){return this.enterChild(1,0,4)}lastChild(){return this.enterChild(-1,0,4)}childAfter(e){return this.enterChild(1,e,2)}childBefore(e){return this.enterChild(-1,e,-2)}enter(e,t,n=this.mode){return this.buffer?n&X.ExcludeBuffers?!1:this.enterChild(1,e,t):this.yield(this._tree.enter(e,t,n))}parent(){if(!this.buffer)return this.yieldNode(this.mode&X.IncludeAnonymous?this._tree._parent:this._tree.parent);if(this.stack.length)return this.yieldBuf(this.stack.pop());let e=this.mode&X.IncludeAnonymous?this.buffer.parent:this.buffer.parent.nextSignificantParent();return this.buffer=null,this.yieldNode(e)}sibling(e){if(!this.buffer)return this._tree._parent?this.yield(this._tree.index<0?null:this._tree._parent.nextChild(this._tree.index+e,e,0,4,this.mode)):!1;let{buffer:t}=this.buffer,n=this.stack.length-1;if(e<0){let e=n<0?0:this.stack[n]+4;if(this.index!=e)return this.yieldBuf(t.findChild(e,this.index,-1,0,4))}else{let e=t.buffer[this.index+3];if(e<(n<0?t.buffer.length:t.buffer[this.stack[n]+3]))return this.yieldBuf(e)}return n<0&&this.yield(this.buffer.parent.nextChild(this.buffer.index+e,e,0,4,this.mode))}nextSibling(){return this.sibling(1)}prevSibling(){return this.sibling(-1)}atLastNode(e){let t,n,{buffer:r}=this;if(r){if(e>0){if(this.index<r.buffer.buffer.length)return!1}else for(let e=0;e<this.index;e++)if(r.buffer.buffer[e+3]<this.index)return!1;({index:t,parent:n}=r)}else({index:t,_parent:n}=this._tree);for(;n;{index:t,_parent:n}=n)if(t>-1)for(let r=t+e,i=e<0?-1:n._tree.children.length;r!=i;r+=e){let e=n._tree.children[r];if(this.mode&X.IncludeAnonymous||e instanceof co||!e.type.isAnonymous||So(e))return!1}return!0}move(e,t){if(t&&this.enterChild(e,0,4))return!0;for(;;){if(this.sibling(e))return!0;if(this.atLastNode(e)||!this.parent())return!1}}next(e=!0){return this.move(1,e)}prev(e=!0){return this.move(-1,e)}moveTo(e,t=0){for(;(this.from==this.to||(t<1?this.from>=e:this.from>e)||(t>-1?this.to<=e:this.to<e))&&this.parent(););for(;this.enterChild(1,e,t););return this}get node(){if(!this.buffer)return this._tree;let e=this.bufferNode,t=null,n=0;if(e&&e.context==this.buffer)scan:for(let r=this.index,i=this.stack.length;i>=0;){for(let a=e;a;a=a._parent)if(a.index==r){if(r==this.index)return a;t=a,n=i+1;break scan}r=this.stack[--i]}for(let e=n;e<this.stack.length;e++)t=new _o(this.buffer,t,this.stack[e]);return this.bufferNode=new _o(this.buffer,t,this.index)}get tree(){return this.buffer?null:this._tree._tree}iterate(e,t){for(let n=0;;){let r=!1;if(this.type.isAnonymous||e(this)!==!1){if(this.firstChild()){n++;continue}this.type.isAnonymous||(r=!0)}for(;;){if(r&&t&&t(this),r=this.type.isAnonymous,!n)return;if(this.nextSibling())break;this.parent(),n--,r=!0}}}matchContext(e){if(!this.buffer)return ho(this.node.parent,e);let{buffer:t}=this.buffer,{types:n}=t.set;for(let r=e.length-1,i=this.stack.length-1;r>=0;i--){if(i<0)return ho(this._tree,e,r);let a=n[t.buffer[this.stack[i]]];if(!a.isAnonymous){if(e[r]&&e[r]!=a.name)return!1;r--}}return!0}};function So(e){return e.children.some(e=>e instanceof co||!e.type.isAnonymous||So(e))}function Co(e){let{buffer:t,nodeSet:n,maxBufferLength:r=Qa,reused:i=[],minRepeatType:a=n.types.length}=e,o=Array.isArray(t)?new so(t,t.length):t,s=n.types,c=0,l=0;function u(e,t,_,v,y,b){let{id:x,start:S,end:C,size:w}=o,T=l,E=c;if(w<0){if(o.next(),w==-1){let t=i[x];_.push(t),v.push(S-e);return}if(w==-3){c=x;return}if(w==-4){l=x;return}throw RangeError(`Unrecognized record size: ${w}`)}let ee=s[x],te,D,O=S-e;if(C-S<=r&&(D=h(o.pos-t,y))){let t=new Uint16Array(D.size-D.skip),r=o.pos-D.size,i=t.length;for(;o.pos>r;)i=g(D.start,t,i);te=new co(t,C-D.start,n),O=D.start-e}else{let e=o.pos-w;o.next();let t=[],n=[],i=x>=a?x:-1,s=0,c=C;for(;o.pos>e;)i>=0&&o.id==i&&o.size>=0?(o.end<=c-r&&(p(t,n,S,s,o.end,c,i,T,E),s=t.length,c=o.end),o.next()):b>2500?d(S,e,t,n):u(S,e,t,n,i,b+1);if(i>=0&&s>0&&s<t.length&&p(t,n,S,s,S,c,i,T,E),t.reverse(),n.reverse(),i>-1&&s>0){let e=f(ee,E);te=Eo(ee,t,n,0,t.length,0,C-S,e,e)}else te=m(ee,t,n,C-S,T-C,E)}_.push(te),v.push(O)}function d(e,t,i,a){let s=[],c=0,l=-1;for(;o.pos>t;){let{id:e,start:t,end:n,size:i}=o;if(i>4)o.next();else if(l>-1&&t<l)break;else l<0&&(l=n-r),s.push(e,t,n),c++,o.next()}if(c){let t=new Uint16Array(c*4),r=s[s.length-2];for(let e=s.length-3,n=0;e>=0;e-=3)t[n++]=s[e],t[n++]=s[e+1]-r,t[n++]=s[e+2]-r,t[n++]=n;i.push(new co(t,s[2]-r,n)),a.push(r-e)}}function f(e,t){return(n,r,i)=>{let a=0,o=n.length-1,s,c;if(o>=0&&(s=n[o])instanceof Z){if(!o&&s.type==e&&s.length==i)return s;(c=s.prop(Y.lookAhead))&&(a=r[o]+s.length+c)}return m(e,n,r,i,a,t)}}function p(e,t,r,i,a,o,s,c,l){let u=[],d=[];for(;e.length>i;)u.push(e.pop()),d.push(t.pop()+r-a);e.push(m(n.types[s],u,d,o-a,c-o,l)),t.push(a-r)}function m(e,t,n,r,i,a,o){if(a){let e=[Y.contextHash,a];o=o?[e].concat(o):[e]}if(i>25){let e=[Y.lookAhead,i];o=o?[e].concat(o):[e]}return new Z(e,t,n,r,o)}function h(e,t){let n=o.fork(),i=0,s=0,c=0,l=n.end-r,u={size:0,start:0,skip:0};scan:for(let r=n.pos-e;n.pos>r;){let e=n.size;if(n.id==t&&e>=0){u.size=i,u.start=s,u.skip=c,c+=4,i+=4,n.next();continue}let o=n.pos-e;if(e<0||o<r||n.start<l)break;let d=n.id>=a?4:0,f=n.start;for(n.next();n.pos>o;){if(n.size<0){if(n.size==-3||n.size==-4)d+=4;else break scan}else n.id>=a&&(d+=4);n.next()}s=f,i+=e,c+=d}return(t<0||i==e)&&(u.size=i,u.start=s,u.skip=c),u.size>4?u:void 0}function g(e,t,n){let{id:r,start:i,end:s,size:u}=o;if(o.next(),u>=0&&r<a){let a=n;if(u>4){let r=o.pos-(u-4);for(;o.pos>r;)n=g(e,t,n)}t[--n]=a,t[--n]=s-e,t[--n]=i-e,t[--n]=r}else u==-3?c=r:u==-4&&(l=r);return n}let _=[],v=[];for(;o.pos>0;)u(e.start||0,e.bufferStart||0,_,v,-1,0);let y=e.length??(_.length?v[0]+_[0].length:0);return new Z(s[e.topID],_.reverse(),v.reverse(),y)}var wo=new WeakMap;function To(e,t){if(!e.isAnonymous||t instanceof co||t.type!=e)return 1;let n=wo.get(t);if(n==null){n=1;for(let r of t.children){if(r.type!=e||!(r instanceof Z)){n=1;break}n+=To(e,r)}wo.set(t,n)}return n}function Eo(e,t,n,r,i,a,o,s,c){let l=0;for(let n=r;n<i;n++)l+=To(e,t[n]);let u=Math.ceil(l*1.5/8),d=[],f=[];function p(t,n,r,i,o){for(let s=r;s<i;){let r=s,l=n[s],m=To(e,t[s]);for(s++;s<i;s++){let n=To(e,t[s]);if(m+n>=u)break;m+=n}if(s==r+1){if(m>u){let e=t[r];p(e.children,e.positions,0,e.children.length,n[r]+o);continue}d.push(t[r])}else{let i=n[s-1]+t[s-1].length-l;d.push(Eo(e,t,n,r,s,l,i,null,c))}f.push(l+o-a)}}return p(t,n,r,i,0),(s||c)(d,f,o)}var Do=class{constructor(){this.map=new WeakMap}setBuffer(e,t,n){let r=this.map.get(e);r||this.map.set(e,r=new Map),r.set(t,n)}getBuffer(e,t){let n=this.map.get(e);return n&&n.get(t)}set(e,t){e instanceof _o?this.setBuffer(e.context.buffer,e.index,t):e instanceof po&&this.map.set(e.tree,t)}get(e){return e instanceof _o?this.getBuffer(e.context.buffer,e.index):e instanceof po?this.map.get(e.tree):void 0}cursorSet(e,t){e.buffer?this.setBuffer(e.buffer.buffer,e.index,t):this.map.set(e.tree,t)}cursorGet(e){return e.buffer?this.getBuffer(e.buffer.buffer,e.index):this.map.get(e.tree)}},Oo=class e{constructor(e,t,n,r,i=!1,a=!1){this.from=e,this.to=t,this.tree=n,this.offset=r,this.open=!!i|(a?2:0)}get openStart(){return(this.open&1)>0}get openEnd(){return(this.open&2)>0}static addTree(t,n=[],r=!1){let i=[new e(0,t.length,t,0,!1,r)];for(let e of n)e.to>t.length&&i.push(e);return i}static applyChanges(t,n,r=128){if(!n.length)return t;let i=[],a=1,o=t.length?t[0]:null;for(let s=0,c=0,l=0;;s++){let u=s<n.length?n[s]:null,d=u?u.fromA:1e9;if(d-c>=r)for(;o&&o.from<d;){let n=o;if(c>=n.from||d<=n.to||l){let t=Math.max(n.from,c)-l,r=Math.min(n.to,d)-l;n=t>=r?null:new e(t,r,n.tree,n.offset+l,s>0,!!u)}if(n&&i.push(n),o.to>d)break;o=a<t.length?t[a++]:null}if(!u)break;c=u.toA,l=u.toA-u.toB}return i}},ko=class{startParse(e,t,n){return typeof e==`string`&&(e=new Ao(e)),n=n?n.length?n.map(e=>new eo(e.from,e.to)):[new eo(0,0)]:[new eo(0,e.length)],this.createParse(e,t||[],n)}parse(e,t,n){let r=this.startParse(e,t,n);for(;;){let e=r.advance();if(e)return e}}},Ao=class{constructor(e){this.string=e}get length(){return this.string.length}chunk(e){return this.string.slice(e)}get lineChunks(){return!1}read(e,t){return this.string.slice(e,t)}};function jo(e){return(t,n,r,i)=>new Io(t,e,n,r,i)}var Mo=class{constructor(e,t,n,r,i,a){this.parser=e,this.parse=t,this.overlay=n,this.bracketed=r,this.target=i,this.from=a}};function No(e){if(!e.length||e.some(e=>e.from>=e.to))throw RangeError(`Invalid inner parse ranges given: `+JSON.stringify(e))}var Po=class{constructor(e,t,n,r,i,a,o,s){this.parser=e,this.predicate=t,this.mounts=n,this.index=r,this.start=i,this.bracketed=a,this.target=o,this.prev=s,this.depth=0,this.ranges=[]}},Fo=new Y({perNode:!0}),Io=class{constructor(e,t,n,r,i){this.nest=t,this.input=n,this.fragments=r,this.ranges=i,this.inner=[],this.innerDone=0,this.baseTree=null,this.stoppedAt=null,this.baseParse=e}advance(){if(this.baseParse){let e=this.baseParse.advance();if(!e)return null;if(this.baseParse=null,this.baseTree=e,this.startInner(),this.stoppedAt!=null)for(let e of this.inner)e.parse.stopAt(this.stoppedAt)}if(this.innerDone==this.inner.length){let e=this.baseTree;return this.stoppedAt!=null&&(e=new Z(e.type,e.children,e.positions,e.length,e.propValues.concat([[Fo,this.stoppedAt]]))),e}let e=this.inner[this.innerDone],t=e.parse.advance();if(t){this.innerDone++;let n=Object.assign(Object.create(null),e.target.props);n[Y.mounted.id]=new to(t,e.overlay,e.parser,e.bracketed),e.target.props=n}return null}get parsedPos(){if(this.baseParse)return 0;let e=this.input.length;for(let t=this.innerDone;t<this.inner.length;t++)this.inner[t].from<e&&(e=Math.min(e,this.inner[t].parse.parsedPos));return e}stopAt(e){if(this.stoppedAt=e,this.baseParse)this.baseParse.stopAt(e);else for(let t=this.innerDone;t<this.inner.length;t++)this.inner[t].parse.stopAt(e)}startInner(){let e=new Vo(this.fragments),t=null,n=null,r=new xo(new po(this.baseTree,this.ranges[0].from,0,null),X.IncludeAnonymous|X.IgnoreMounts);scan:for(let i,a;;){let o=!0,s;if(this.stoppedAt!=null&&r.from>=this.stoppedAt)o=!1;else if(e.hasNode(r)){if(t){let e=t.mounts.find(e=>e.frag.from<=r.from&&e.frag.to>=r.to&&e.mount.overlay);if(e)for(let n of e.mount.overlay){let i=n.from+e.pos,a=n.to+e.pos;i>=r.from&&a<=r.to&&!t.ranges.some(e=>e.from<a&&e.to>i)&&t.ranges.push({from:i,to:a})}}o=!1}else if(n&&(a=Lo(n.ranges,r.from,r.to)))o=a!=2;else if(!r.type.isAnonymous&&(i=this.nest(r,this.input))&&(r.from<r.to||!i.overlay)){r.tree||(zo(r),t&&t.depth++,n&&n.depth++);let a=e.findMounts(r.from,i.parser);if(typeof i.overlay==`function`)t=new Po(i.parser,i.overlay,a,this.inner.length,r.from,!!i.bracketed,r.tree,t);else{let e=Ho(this.ranges,i.overlay||(r.from<r.to?[new eo(r.from,r.to)]:[]));e.length&&No(e),(e.length||!i.overlay)&&this.inner.push(new Mo(i.parser,e.length?i.parser.startParse(this.input,Wo(a,e),e):i.parser.startParse(``),i.overlay?i.overlay.map(e=>new eo(e.from-r.from,e.to-r.from)):null,!!i.bracketed,r.tree,e.length?e[0].from:r.from)),i.overlay?e.length&&(n={ranges:e,depth:0,prev:n}):o=!1}}else if(t&&(s=t.predicate(r))&&(s===!0&&(s=new eo(r.from,r.to)),s.from<s.to)){let e=t.ranges.length-1;e>=0&&t.ranges[e].to==s.from?t.ranges[e]={from:t.ranges[e].from,to:s.to}:t.ranges.push(s)}if(o&&r.firstChild())t&&t.depth++,n&&n.depth++;else for(;!r.nextSibling();){if(!r.parent())break scan;if(t&&!--t.depth){let e=Ho(this.ranges,t.ranges);e.length&&(No(e),this.inner.splice(t.index,0,new Mo(t.parser,t.parser.startParse(this.input,Wo(t.mounts,e),e),t.ranges.map(e=>new eo(e.from-t.start,e.to-t.start)),t.bracketed,t.target,e[0].from))),t=t.prev}n&&!--n.depth&&(n=n.prev)}}}};function Lo(e,t,n){for(let r of e){if(r.from>=n)break;if(r.to>t)return r.from<=t&&r.to>=n?2:1}return 0}function Ro(e,t,n,r,i,a){if(t<n){let o=e.buffer[t+1];r.push(e.slice(t,n,o)),i.push(o-a)}}function zo(e){let{node:t}=e,n=[],r=t.context.buffer;do n.push(e.index),e.parent();while(!e.tree);let i=e.tree,a=i.children.indexOf(r),o=i.children[a],s=o.buffer,c=[a];function l(e,r,i,a,u,d){let f=n[d],p=[],m=[];Ro(o,e,f,p,m,a);let h=s[f+1],g=s[f+2];c.push(p.length);let _=d?l(f+4,s[f+3],o.set.types[s[f]],h,g-h,d-1):t.toTree();return p.push(_),m.push(h-a),Ro(o,s[f+3],r,p,m,a),new Z(i,p,m,u)}i.children[a]=l(0,s.length,ro.none,0,o.length,n.length-1);for(let t of c){let n=e.tree.children[t],r=e.tree.positions[t];e.yield(new po(n,r+e.from,t,e._tree))}}var Bo=class{constructor(e,t){this.offset=t,this.done=!1,this.cursor=e.cursor(X.IncludeAnonymous|X.IgnoreMounts)}moveTo(e){let{cursor:t}=this,n=e-this.offset;for(;!this.done&&t.from<n;)if(!(t.to>=e&&t.enter(n,1,X.IgnoreOverlays|X.ExcludeBuffers))){if(t.to<=e)t.next(!1)||(this.done=!0);else break}}hasNode(e){if(this.moveTo(e.from),!this.done&&this.cursor.from+this.offset==e.from&&this.cursor.tree)for(let t=this.cursor.tree;;){if(t==e.tree)return!0;if(t.children.length&&t.positions[0]==0&&t.children[0]instanceof Z)t=t.children[0];else break}return!1}},Vo=class{constructor(e){if(this.fragments=e,this.curTo=0,this.fragI=0,e.length){let t=this.curFrag=e[0];this.curTo=t.tree.prop(Fo)??t.to,this.inner=new Bo(t.tree,-t.offset)}else this.curFrag=this.inner=null}hasNode(e){for(;this.curFrag&&e.from>=this.curTo;)this.nextFrag();return this.curFrag&&this.curFrag.from<=e.from&&this.curTo>=e.to&&this.inner.hasNode(e)}nextFrag(){if(this.fragI++,this.fragI==this.fragments.length)this.curFrag=this.inner=null;else{let e=this.curFrag=this.fragments[this.fragI];this.curTo=e.tree.prop(Fo)??e.to,this.inner=new Bo(e.tree,-e.offset)}}findMounts(e,t){let n=[];if(this.inner){this.inner.cursor.moveTo(e,1);for(let e=this.inner.cursor.node;e;e=e.parent){let r=e.tree?.prop(Y.mounted);if(r&&r.parser==t)for(let t=this.fragI;t<this.fragments.length;t++){let i=this.fragments[t];if(i.from>=e.to)break;i.tree==this.curFrag.tree&&n.push({frag:i,pos:e.from-i.offset,mount:r})}}}return n}};function Ho(e,t){let n=null,r=t;for(let i=1,a=0;i<e.length;i++){let o=e[i-1].to,s=e[i].from;for(;a<r.length;a++){let e=r[a];if(e.from>=s)break;e.to<=o||(n||(r=n=t.slice()),e.from<o?(n[a]=new eo(e.from,o),e.to>s&&n.splice(a+1,0,new eo(s,e.to))):e.to>s?n[a--]=new eo(s,e.to):n.splice(a--,1))}}return r}function Uo(e,t,n,r){let i=0,a=0,o=!1,s=!1,c=-1e9,l=[];for(;;){let u=i==e.length?1e9:o?e[i].to:e[i].from,d=a==t.length?1e9:s?t[a].to:t[a].from;if(o!=s){let e=Math.max(c,n),t=Math.min(u,d,r);e<t&&l.push(new eo(e,t))}if(c=Math.min(u,d),c==1e9)break;u==c&&(o?(o=!1,i++):o=!0),d==c&&(s?(s=!1,a++):s=!0)}return l}function Wo(e,t){let n=[];for(let{pos:r,mount:i,frag:a}of e){let e=r+(i.overlay?i.overlay[0].from:0),o=e+i.tree.length,s=Math.max(a.from,e),c=Math.min(a.to,o);if(i.overlay){let o=Uo(t,i.overlay.map(e=>new eo(e.from+r,e.to+r)),s,c);for(let t=0,r=s;;t++){let s=t==o.length,l=s?c:o[t].from;if(l>r&&n.push(new Oo(r,l,i.tree,-e,a.from>=r||a.openStart,a.to<=l||a.openEnd)),s)break;r=o[t].to}}else n.push(new Oo(s,c,i.tree,-e,a.from>=e||a.openStart,a.to<=o||a.openEnd))}return n}var Go=class e{constructor(e,t,n,r,i,a,o,s,c,l=0,u){this.p=e,this.stack=t,this.state=n,this.reducePos=r,this.pos=i,this.score=a,this.buffer=o,this.bufferBase=s,this.curContext=c,this.lookAhead=l,this.parent=u}toString(){return`[${this.stack.filter((e,t)=>t%3==0).concat(this.state)}]@${this.pos}${this.score?`!`+this.score:``}`}static start(t,n,r=0){let i=t.parser.context;return new e(t,[],n,r,r,0,[],0,i?new Ko(i,i.start):null,0,null)}get context(){return this.curContext?this.curContext.context:null}pushState(e,t){this.stack.push(this.state,t,this.bufferBase+this.buffer.length),this.state=e}reduce(e){let t=e>>19,n=e&65535,{parser:r}=this.p,i=this.reducePos<this.pos-25&&this.setLookAhead(this.pos),a=r.dynamicPrecedence(n);if(a&&(this.score+=a),t==0){n<r.minRepeatTerm&&this.reducePos<this.pos&&(this.reducePos=this.pos),this.pushState(r.getGoto(this.state,n,!0),this.reducePos),n<r.minRepeatTerm&&this.storeNode(n,this.reducePos,this.reducePos,i?8:4,!0),this.reduceContext(n,this.reducePos);return}let o=this.stack.length-(t-1)*3-(e&262144?6:0),s=o?this.stack[o-2]:this.p.ranges[0].from;n<r.minRepeatTerm&&s==this.reducePos&&this.reducePos<this.pos&&(this.reducePos=this.pos);let c=this.reducePos-s;c>=2e3&&!this.p.parser.nodeSet.types[n]?.isAnonymous&&(s==this.p.lastBigReductionStart?(this.p.bigReductionCount++,this.p.lastBigReductionSize=c):this.p.lastBigReductionSize<c&&(this.p.bigReductionCount=1,this.p.lastBigReductionStart=s,this.p.lastBigReductionSize=c));let l=o?this.stack[o-1]:0,u=this.bufferBase+this.buffer.length-l;if(n<r.minRepeatTerm||e&131072){let e=r.stateFlag(this.state,1)?this.pos:this.reducePos;this.storeNode(n,s,e,u+4,!0)}if(e&262144)this.state=this.stack[o];else{let e=this.stack[o-3];this.state=r.getGoto(e,n,!0)}for(;this.stack.length>o;)this.stack.pop();this.reduceContext(n,s)}storeNode(e,t,n,r=4,i=!1){if(e==0&&(!this.stack.length||this.stack[this.stack.length-1]<this.buffer.length+this.bufferBase)){let e=this.buffer.length;if(e>0&&this.buffer[e-4]==0&&this.buffer[e-1]>-1){if(t==n)return;if(this.buffer[e-2]>=t){this.buffer[e-2]=n;return}}}if(!i||this.pos==n)this.buffer.push(e,t,n,r);else{let i=this.buffer.length;if(i>0&&(this.buffer[i-4]!=0||this.buffer[i-1]<0)){let e=!1;for(let t=i;t>0&&this.buffer[t-2]>n;t-=4)if(this.buffer[t-1]>=0){e=!0;break}if(e)for(;i>0&&this.buffer[i-2]>n;)this.buffer[i]=this.buffer[i-4],this.buffer[i+1]=this.buffer[i-3],this.buffer[i+2]=this.buffer[i-2],this.buffer[i+3]=this.buffer[i-1],i-=4,r>4&&(r-=4)}this.buffer[i]=e,this.buffer[i+1]=t,this.buffer[i+2]=n,this.buffer[i+3]=r}}shift(e,t,n,r){if(e&131072)this.pushState(e&65535,this.pos);else if(e&262144)this.pos=r,this.shiftContext(t,n),t<=this.p.parser.maxNode&&this.buffer.push(t,n,r,4);else{let i=e,{parser:a}=this.p;this.pos=r;let o=a.stateFlag(i,1);!o&&(r>n||t<=a.maxNode)&&(this.reducePos=r),this.pushState(i,o?n:Math.min(n,this.reducePos)),this.shiftContext(t,n),t<=a.maxNode&&this.buffer.push(t,n,r,4)}}apply(e,t,n,r){e&65536?this.reduce(e):this.shift(e,t,n,r)}useNode(e,t){let n=this.p.reused.length-1;(n<0||this.p.reused[n]!=e)&&(this.p.reused.push(e),n++);let r=this.pos;this.reducePos=this.pos=r+e.length,this.pushState(t,r),this.buffer.push(n,r,this.reducePos,-1),this.curContext&&this.updateContext(this.curContext.tracker.reuse(this.curContext.context,e,this,this.p.stream.reset(this.pos-e.length)))}split(){let t=this,n=t.buffer.length;for(n&&t.buffer[n-4]==0&&(n-=4);n>0&&t.buffer[n-2]>t.reducePos;)n-=4;let r=t.buffer.slice(n),i=t.bufferBase+n;for(;t&&i==t.bufferBase;)t=t.parent;return new e(this.p,this.stack.slice(),this.state,this.reducePos,this.pos,this.score,r,i,this.curContext,this.lookAhead,t)}recoverByDelete(e,t){let n=e<=this.p.parser.maxNode;n&&this.storeNode(e,this.pos,t,4),this.storeNode(0,this.pos,t,n?8:4),this.pos=this.reducePos=t,this.score-=190}canShift(e){for(let t=new qo(this);;){let n=this.p.parser.stateSlot(t.state,4)||this.p.parser.hasAction(t.state,e);if(n==0)return!1;if(!(n&65536))return!0;t.reduce(n)}}recoverByInsert(e){if(this.stack.length>=300)return[];let t=this.p.parser.nextStates(this.state);if(t.length>8||this.stack.length>=120){let n=[];for(let r=0,i;r<t.length;r+=2)(i=t[r+1])!=this.state&&this.p.parser.hasAction(i,e)&&n.push(t[r],i);if(this.stack.length<120)for(let e=0;n.length<8&&e<t.length;e+=2){let r=t[e+1];n.some((e,t)=>t&1&&e==r)||n.push(t[e],r)}t=n}let n=[];for(let e=0;e<t.length&&n.length<4;e+=2){let r=t[e+1];if(r==this.state)continue;let i=this.split();i.pushState(r,this.pos),i.storeNode(0,i.pos,i.pos,4,!0),i.shiftContext(t[e],this.pos),i.reducePos=this.pos,i.score-=200,n.push(i)}return n}forceReduce(){let{parser:e}=this.p,t=e.stateSlot(this.state,5);if(!(t&65536))return!1;if(!e.validAction(this.state,t)){let n=t>>19,r=t&65535,i=this.stack.length-n*3;if(i<0||e.getGoto(this.stack[i],r,!1)<0){let e=this.findForcedReduction();if(e==null)return!1;t=e}this.storeNode(0,this.pos,this.pos,4,!0),this.score-=100}return this.reducePos=this.pos,this.reduce(t),!0}findForcedReduction(){let{parser:e}=this.p,t=[],n=(r,i)=>{if(!t.includes(r))return t.push(r),e.allActions(r,t=>{if(!(t&393216)){if(t&65536){let n=(t>>19)-i;if(n>1){let r=t&65535,i=this.stack.length-n*3;if(i>=0&&e.getGoto(this.stack[i],r,!1)>=0)return n<<19|65536|r}}else{let e=n(t,i+1);if(e!=null)return e}}})};return n(this.state,0)}forceAll(){for(;!this.p.parser.stateFlag(this.state,2);)if(!this.forceReduce()){this.storeNode(0,this.pos,this.pos,4,!0);break}return this}get deadEnd(){if(this.stack.length!=3)return!1;let{parser:e}=this.p;return e.data[e.stateSlot(this.state,1)]==65535&&!e.stateSlot(this.state,4)}restart(){this.storeNode(0,this.pos,this.pos,4,!0),this.state=this.stack[0],this.stack.length=0}sameState(e){if(this.state!=e.state||this.stack.length!=e.stack.length)return!1;for(let t=0;t<this.stack.length;t+=3)if(this.stack[t]!=e.stack[t])return!1;return!0}get parser(){return this.p.parser}dialectEnabled(e){return this.p.parser.dialect.flags[e]}shiftContext(e,t){this.curContext&&this.updateContext(this.curContext.tracker.shift(this.curContext.context,e,this,this.p.stream.reset(t)))}reduceContext(e,t){this.curContext&&this.updateContext(this.curContext.tracker.reduce(this.curContext.context,e,this,this.p.stream.reset(t)))}emitContext(){let e=this.buffer.length-1;(e<0||this.buffer[e]!=-3)&&this.buffer.push(this.curContext.hash,this.pos,this.pos,-3)}emitLookAhead(){let e=this.buffer.length-1;(e<0||this.buffer[e]!=-4)&&this.buffer.push(this.lookAhead,this.pos,this.pos,-4)}updateContext(e){if(e!=this.curContext.context){let t=new Ko(this.curContext.tracker,e);t.hash!=this.curContext.hash&&this.emitContext(),this.curContext=t}}setLookAhead(e){return e<=this.lookAhead?!1:(this.emitLookAhead(),this.lookAhead=e,!0)}close(){this.curContext&&this.curContext.tracker.strict&&this.emitContext(),this.lookAhead>0&&this.emitLookAhead()}},Ko=class{constructor(e,t){this.tracker=e,this.context=t,this.hash=e.strict?e.hash(t):0}},qo=class{constructor(e){this.start=e,this.state=e.state,this.stack=e.stack,this.base=this.stack.length}reduce(e){let t=e&65535,n=e>>19;n==0?(this.stack==this.start.stack&&(this.stack=this.stack.slice()),this.stack.push(this.state,0,0),this.base+=3):this.base-=(n-1)*3;let r=this.start.p.parser.getGoto(this.stack[this.base-3],t,!0);this.state=r}},Jo=class e{constructor(e,t,n){this.stack=e,this.pos=t,this.index=n,this.buffer=e.buffer,this.index==0&&this.maybeNext()}static create(t,n=t.bufferBase+t.buffer.length){return new e(t,n,n-t.bufferBase)}maybeNext(){let e=this.stack.parent;e!=null&&(this.index=this.stack.bufferBase-e.bufferBase,this.stack=e,this.buffer=e.buffer)}get id(){return this.buffer[this.index-4]}get start(){return this.buffer[this.index-3]}get end(){return this.buffer[this.index-2]}get size(){return this.buffer[this.index-1]}next(){this.index-=4,this.pos-=4,this.index==0&&this.maybeNext()}fork(){return new e(this.stack,this.pos,this.index)}};function Yo(e,t=Uint16Array){if(typeof e!=`string`)return e;let n=null;for(let r=0,i=0;r<e.length;){let a=0;for(;;){let t=e.charCodeAt(r++),n=!1;if(t==126){a=65535;break}t>=92&&t--,t>=34&&t--;let i=t-32;if(i>=46&&(i-=46,n=!0),a+=i,n)break;a*=46}n?n[i++]=a:n=new t(a)}return n}var Xo=class{constructor(){this.start=-1,this.value=-1,this.end=-1,this.extended=-1,this.lookAhead=0,this.mask=0,this.context=0}},Zo=new Xo,Qo=class{constructor(e,t){this.input=e,this.ranges=t,this.chunk=``,this.chunkOff=0,this.chunk2=``,this.chunk2Pos=0,this.next=-1,this.token=Zo,this.rangeIndex=0,this.pos=this.chunkPos=t[0].from,this.range=t[0],this.end=t[t.length-1].to,this.readNext()}resolveOffset(e,t){let n=this.range,r=this.rangeIndex,i=this.pos+e;for(;i<n.from;){if(!r)return null;let e=this.ranges[--r];i-=n.from-e.to,n=e}for(;t<0?i>n.to:i>=n.to;){if(r==this.ranges.length-1)return null;let e=this.ranges[++r];i+=e.from-n.to,n=e}return i}clipPos(e){if(e>=this.range.from&&e<this.range.to)return e;for(let t of this.ranges)if(t.to>e)return Math.max(e,t.from);return this.end}peek(e){let t=this.chunkOff+e,n,r;if(t>=0&&t<this.chunk.length)n=this.pos+e,r=this.chunk.charCodeAt(t);else{let t=this.resolveOffset(e,1);if(t==null)return-1;if(n=t,n>=this.chunk2Pos&&n<this.chunk2Pos+this.chunk2.length)r=this.chunk2.charCodeAt(n-this.chunk2Pos);else{let e=this.rangeIndex,t=this.range;for(;t.to<=n;)t=this.ranges[++e];this.chunk2=this.input.chunk(this.chunk2Pos=n),n+this.chunk2.length>t.to&&(this.chunk2=this.chunk2.slice(0,t.to-n)),r=this.chunk2.charCodeAt(0)}}return n>=this.token.lookAhead&&(this.token.lookAhead=n+1),r}acceptToken(e,t=0){let n=t?this.resolveOffset(t,-1):this.pos;if(n==null||n<this.token.start)throw RangeError(`Token end out of bounds`);this.token.value=e,this.token.end=n}acceptTokenTo(e,t){this.token.value=e,this.token.end=t}getChunk(){if(this.pos>=this.chunk2Pos&&this.pos<this.chunk2Pos+this.chunk2.length){let{chunk:e,chunkPos:t}=this;this.chunk=this.chunk2,this.chunkPos=this.chunk2Pos,this.chunk2=e,this.chunk2Pos=t,this.chunkOff=this.pos-this.chunkPos}else{this.chunk2=this.chunk,this.chunk2Pos=this.chunkPos;let e=this.input.chunk(this.pos),t=this.pos+e.length;this.chunk=t>this.range.to?e.slice(0,this.range.to-this.pos):e,this.chunkPos=this.pos,this.chunkOff=0}}readNext(){return this.next=this.chunkOff>=this.chunk.length&&(this.getChunk(),this.chunkOff==this.chunk.length)?-1:this.chunk.charCodeAt(this.chunkOff)}advance(e=1){for(this.chunkOff+=e;this.pos+e>=this.range.to;){if(this.rangeIndex==this.ranges.length-1)return this.setDone();e-=this.range.to-this.pos,this.range=this.ranges[++this.rangeIndex],this.pos=this.range.from}return this.pos+=e,this.pos>=this.token.lookAhead&&(this.token.lookAhead=this.pos+1),this.readNext()}setDone(){return this.pos=this.chunkPos=this.end,this.range=this.ranges[this.rangeIndex=this.ranges.length-1],this.chunk=``,this.next=-1}reset(e,t){if(t?(this.token=t,t.start=e,t.lookAhead=e+1,t.value=t.extended=-1):this.token=Zo,this.pos!=e){if(this.pos=e,e==this.end)return this.setDone(),this;for(;e<this.range.from;)this.range=this.ranges[--this.rangeIndex];for(;e>=this.range.to;)this.range=this.ranges[++this.rangeIndex];e>=this.chunkPos&&e<this.chunkPos+this.chunk.length?this.chunkOff=e-this.chunkPos:(this.chunk=``,this.chunkOff=0),this.readNext()}return this}read(e,t){if(e>=this.chunkPos&&t<=this.chunkPos+this.chunk.length)return this.chunk.slice(e-this.chunkPos,t-this.chunkPos);if(e>=this.chunk2Pos&&t<=this.chunk2Pos+this.chunk2.length)return this.chunk2.slice(e-this.chunk2Pos,t-this.chunk2Pos);if(e>=this.range.from&&t<=this.range.to)return this.input.read(e,t);let n=``;for(let r of this.ranges){if(r.from>=t)break;r.to>e&&(n+=this.input.read(Math.max(r.from,e),Math.min(r.to,t)))}return n}},$o=class{constructor(e,t){this.data=e,this.id=t}token(e,t){let{parser:n}=t.p;ns(this.data,e,t,this.id,n.data,n.tokenPrecTable)}};$o.prototype.contextual=$o.prototype.fallback=$o.prototype.extend=!1;var es=class{constructor(e,t,n){this.precTable=t,this.elseToken=n,this.data=typeof e==`string`?Yo(e):e}token(e,t){let n=e.pos,r=0;for(;;){let n=e.next<0,i=e.resolveOffset(1,1);if(ns(this.data,e,t,0,this.data,this.precTable),e.token.value>-1)break;if(this.elseToken==null)return;if(n||r++,i==null)break;e.reset(i,e.token)}r&&(e.reset(n,e.token),e.acceptToken(this.elseToken,r))}};es.prototype.contextual=$o.prototype.fallback=$o.prototype.extend=!1;var ts=class{constructor(e,t={}){this.token=e,this.contextual=!!t.contextual,this.fallback=!!t.fallback,this.extend=!!t.extend}};function ns(e,t,n,r,i,a){let o=0,s=1<<r,{dialect:c}=n.p.parser;scan:for(;(s&e[o])!=0;){let n=e[o+1];for(let r=o+3;r<n;r+=2)if((e[r+1]&s)>0){let n=e[r];if(c.allows(n)&&(t.token.value==-1||t.token.value==n||is(n,t.token.value,i,a))){t.acceptToken(n);break}}let r=t.next,l=0,u=e[o+2];if(t.next<0&&u>l&&e[n+u*3-3]==65535){o=e[n+u*3-1];continue scan}for(;l<u;){let i=l+u>>1,a=n+i+(i<<1),s=e[a],c=e[a+1]||65536;if(r<s)u=i;else if(r>=c)l=i+1;else{o=e[a+2],t.advance();continue scan}}break}}function rs(e,t,n){for(let r=t,i;(i=e[r])!=65535;r++)if(i==n)return r-t;return-1}function is(e,t,n,r){let i=rs(n,r,t);return i<0||rs(n,r,e)<i}var Q=typeof process<`u`&&/\bparse\b/.test({}.LOG),as=null;function os(e,t,n){let r=e.cursor(X.IncludeAnonymous);for(r.moveTo(t);;)if(!(n<0?r.childBefore(t):r.childAfter(t)))for(;;){if((n<0?r.to<t:r.from>t)&&!r.type.isError)return n<0?Math.max(0,Math.min(r.to-1,t-25)):Math.min(e.length,Math.max(r.from+1,t+25));if(n<0?r.prevSibling():r.nextSibling())break;if(!r.parent())return n<0?0:e.length}}var ss=class{constructor(e,t){this.fragments=e,this.nodeSet=t,this.i=0,this.fragment=null,this.safeFrom=-1,this.safeTo=-1,this.trees=[],this.start=[],this.index=[],this.nextFragment()}nextFragment(){let e=this.fragment=this.i==this.fragments.length?null:this.fragments[this.i++];if(e){for(this.safeFrom=e.openStart?os(e.tree,e.from+e.offset,1)-e.offset:e.from,this.safeTo=e.openEnd?os(e.tree,e.to+e.offset,-1)-e.offset:e.to;this.trees.length;)this.trees.pop(),this.start.pop(),this.index.pop();this.trees.push(e.tree),this.start.push(-e.offset),this.index.push(0),this.nextStart=this.safeFrom}else this.nextStart=1e9}nodeAt(e){if(e<this.nextStart)return null;for(;this.fragment&&this.safeTo<=e;)this.nextFragment();if(!this.fragment)return null;for(;;){let t=this.trees.length-1;if(t<0)return this.nextFragment(),null;let n=this.trees[t],r=this.index[t];if(r==n.children.length){this.trees.pop(),this.start.pop(),this.index.pop();continue}let i=n.children[r],a=this.start[t]+n.positions[r];if(a>e)return this.nextStart=a,null;if(i instanceof Z){if(a==e){if(a<this.safeFrom)return null;let e=a+i.length;if(e<=this.safeTo){let t=i.prop(Y.lookAhead);if(!t||e+t<this.fragment.to)return i}}this.index[t]++,a+i.length>=Math.max(this.safeFrom,e)&&(this.trees.push(i),this.start.push(a),this.index.push(0))}else this.index[t]++,this.nextStart=a+i.length}}},cs=class{constructor(e,t){this.stream=t,this.tokens=[],this.mainToken=null,this.actions=[],this.tokens=e.tokenizers.map(e=>new Xo)}getActions(e){let t=0,n=null,{parser:r}=e.p,{tokenizers:i}=r,a=r.stateSlot(e.state,3),o=e.curContext?e.curContext.hash:0,s=0;for(let r=0;r<i.length;r++){if(!(1<<r&a))continue;let c=i[r],l=this.tokens[r];if((!n||c.fallback)&&((c.contextual||l.start!=e.pos||l.mask!=a||l.context!=o)&&(this.updateCachedToken(l,c,e),l.mask=a,l.context=o),l.lookAhead>l.end+25&&(s=Math.max(l.lookAhead,s)),l.value!=0)){let r=t;if(l.extended>-1&&(t=this.addActions(e,l.extended,l.end,t)),t=this.addActions(e,l.value,l.end,t),!c.extend&&(n=l,t>r))break}}for(;this.actions.length>t;)this.actions.pop();return s&&e.setLookAhead(s),!n&&e.pos==this.stream.end&&(n=new Xo,n.value=e.p.parser.eofTerm,n.start=n.end=e.pos,t=this.addActions(e,n.value,n.end,t)),this.mainToken=n,this.actions}getMainToken(e){if(this.mainToken)return this.mainToken;let t=new Xo,{pos:n,p:r}=e;return t.start=n,t.end=Math.min(n+1,r.stream.end),t.value=n==r.stream.end?r.parser.eofTerm:0,t}updateCachedToken(e,t,n){let r=this.stream.clipPos(n.pos);if(t.token(this.stream.reset(r,e),n),e.value>-1){let{parser:t}=n.p;for(let r=0;r<t.specialized.length;r++)if(t.specialized[r]==e.value){let i=t.specializers[r](this.stream.read(e.start,e.end),n);if(i>=0&&n.p.parser.dialect.allows(i>>1)){i&1?e.extended=i>>1:e.value=i>>1;break}}}else e.value=0,e.end=this.stream.clipPos(r+1)}putAction(e,t,n,r){for(let t=0;t<r;t+=3)if(this.actions[t]==e)return r;return this.actions[r++]=e,this.actions[r++]=t,this.actions[r++]=n,r}addActions(e,t,n,r){let{state:i}=e,{parser:a}=e.p,{data:o}=a;for(let e=0;e<2;e++)for(let s=a.stateSlot(i,e?2:1);;s+=3){if(o[s]==65535){if(o[s+1]==1)s=hs(o,s+2);else{r==0&&o[s+1]==2&&(r=this.putAction(hs(o,s+2),t,n,r));break}}o[s]==t&&(r=this.putAction(hs(o,s+1),t,n,r))}return r}},ls=class{constructor(e,t,n,r){this.parser=e,this.input=t,this.ranges=r,this.recovering=0,this.nextStackID=9812,this.minStackPos=0,this.reused=[],this.stoppedAt=null,this.lastBigReductionStart=-1,this.lastBigReductionSize=0,this.bigReductionCount=0,this.stream=new Qo(t,r),this.tokens=new cs(e,this.stream),this.topTerm=e.top[1];let{from:i}=r[0];this.stacks=[Go.start(this,e.top[0],i)],this.fragments=n.length&&this.stream.end-i>e.bufferLength*4?new ss(n,e.nodeSet):null}get parsedPos(){return this.minStackPos}advance(){let e=this.stacks,t=this.minStackPos,n=this.stacks=[],r,i;if(this.bigReductionCount>300&&e.length==1){let[t]=e;for(;t.forceReduce()&&t.stack.length&&t.stack[t.stack.length-2]>=this.lastBigReductionStart;);this.bigReductionCount=this.lastBigReductionSize=0}for(let a=0;a<e.length;a++){let o=e[a];for(;;){if(this.tokens.mainToken=null,o.pos>t)n.push(o);else if(this.advanceStack(o,n,e))continue;else{r||(r=[],i=[]),r.push(o);let e=this.tokens.getMainToken(o);i.push(e.value,e.end)}break}}if(!n.length){let e=r&&gs(r);if(e)return Q&&console.log(`Finish with `+this.stackID(e)),this.stackToTree(e);if(this.parser.strict)throw Q&&r&&console.log(`Stuck with token `+(this.tokens.mainToken?this.parser.getName(this.tokens.mainToken.value):`none`)),SyntaxError(`No parse at `+t);this.recovering||=5}if(this.recovering&&r){let e=this.stoppedAt!=null&&r[0].pos>this.stoppedAt?r[0]:this.runRecovery(r,i,n);if(e)return Q&&console.log(`Force-finish `+this.stackID(e)),this.stackToTree(e.forceAll())}if(this.recovering){let e=this.recovering==1?1:this.recovering*3;if(n.length>e)for(n.sort((e,t)=>t.score-e.score);n.length>e;)n.pop();n.some(e=>e.reducePos>t)&&this.recovering--}else if(n.length>1){outer:for(let e=0;e<n.length-1;e++){let t=n[e];for(let r=e+1;r<n.length;r++){let i=n[r];if(t.sameState(i)||t.buffer.length>500&&i.buffer.length>500){if((t.score-i.score||t.buffer.length-i.buffer.length)>0)n.splice(r--,1);else{n.splice(e--,1);continue outer}}}}n.length>12&&(n.sort((e,t)=>t.score-e.score),n.splice(12,n.length-12))}this.minStackPos=n[0].pos;for(let e=1;e<n.length;e++)n[e].pos<this.minStackPos&&(this.minStackPos=n[e].pos);return null}stopAt(e){if(this.stoppedAt!=null&&this.stoppedAt<e)throw RangeError(`Can't move stoppedAt forward`);this.stoppedAt=e}advanceStack(e,t,n){let r=e.pos,{parser:i}=this,a=Q?this.stackID(e)+` -> `:``;if(this.stoppedAt!=null&&r>this.stoppedAt)return e.forceReduce()?e:null;if(this.fragments){let t=e.curContext&&e.curContext.tracker.strict,n=t?e.curContext.hash:0;for(let o=this.fragments.nodeAt(r);o;){let r=this.parser.nodeSet.types[o.type.id]==o.type?i.getGoto(e.state,o.type.id):-1;if(r>-1&&o.length&&(!t||(o.prop(Y.contextHash)||0)==n))return e.useNode(o,r),Q&&console.log(a+this.stackID(e)+` (via reuse of ${i.getName(o.type.id)})`),!0;if(!(o instanceof Z)||o.children.length==0||o.positions[0]>0)break;let s=o.children[0];if(s instanceof Z&&o.positions[0]==0)o=s;else break}}let o=i.stateSlot(e.state,4);if(o>0)return e.reduce(o),Q&&console.log(a+this.stackID(e)+` (via always-reduce ${i.getName(o&65535)})`),!0;if(e.stack.length>=8400)for(;e.stack.length>6e3&&e.forceReduce(););let s=this.tokens.getActions(e);for(let o=0;o<s.length;){let c=s[o++],l=s[o++],u=s[o++],d=o==s.length||!n,f=d?e:e.split(),p=this.tokens.mainToken;if(f.apply(c,l,p?p.start:f.pos,u),Q&&console.log(a+this.stackID(f)+` (via ${c&65536?`reduce of ${i.getName(c&65535)}`:`shift`} for ${i.getName(l)} @ ${r}${f==e?``:`, split`})`),d)return!0;f.pos>r?t.push(f):n.push(f)}return!1}advanceFully(e,t){let n=e.pos;for(;;){if(!this.advanceStack(e,null,null))return!1;if(e.pos>n)return us(e,t),!0}}runRecovery(e,t,n){let r=null,i=!1;for(let a=0;a<e.length;a++){let o=e[a],s=t[a<<1],c=t[(a<<1)+1],l=Q?this.stackID(o)+` -> `:``;if(o.deadEnd&&(i||(i=!0,o.restart(),Q&&console.log(l+this.stackID(o)+` (restarted)`),this.advanceFully(o,n))))continue;let u=o.split(),d=l;for(let e=0;e<10&&u.forceReduce()&&(Q&&console.log(d+this.stackID(u)+` (via force-reduce)`),!this.advanceFully(u,n));e++)Q&&(d=this.stackID(u)+` -> `);for(let e of o.recoverByInsert(s))Q&&console.log(l+this.stackID(e)+` (via recover-insert)`),this.advanceFully(e,n);this.stream.end>o.pos?(c==o.pos&&(c++,s=0),o.recoverByDelete(s,c),Q&&console.log(l+this.stackID(o)+` (via recover-delete ${this.parser.getName(s)})`),us(o,n)):(!r||r.score<u.score)&&(r=u)}return r}stackToTree(e){return e.close(),Z.build({buffer:Jo.create(e),nodeSet:this.parser.nodeSet,topID:this.topTerm,maxBufferLength:this.parser.bufferLength,reused:this.reused,start:this.ranges[0].from,length:e.pos-this.ranges[0].from,minRepeatType:this.parser.minRepeatTerm})}stackID(e){let t=(as||=new WeakMap).get(e);return t||as.set(e,t=String.fromCodePoint(this.nextStackID++)),t+e}};function us(e,t){for(let n=0;n<t.length;n++){let r=t[n];if(r.pos==e.pos&&r.sameState(e)){t[n].score<e.score&&(t[n]=e);return}}t.push(e)}var ds=class{constructor(e,t,n){this.source=e,this.flags=t,this.disabled=n}allows(e){return!this.disabled||this.disabled[e]==0}},fs=e=>e,ps=class{constructor(e){this.start=e.start,this.shift=e.shift||fs,this.reduce=e.reduce||fs,this.reuse=e.reuse||fs,this.hash=e.hash||(()=>0),this.strict=e.strict!==!1}},ms=class e extends ko{constructor(e){if(super(),this.wrappers=[],e.version!=14)throw RangeError(`Parser version (${e.version}) doesn't match runtime version (14)`);let t=e.nodeNames.split(` `);this.minRepeatTerm=t.length;for(let n=0;n<e.repeatNodeCount;n++)t.push(``);let n=Object.keys(e.topRules).map(t=>e.topRules[t][1]),r=[];for(let e=0;e<t.length;e++)r.push([]);function i(e,t,n){r[e].push([t,t.deserialize(String(n))])}if(e.nodeProps)for(let t of e.nodeProps){let e=t[0];typeof e==`string`&&(e=Y[e]);for(let n=1;n<t.length;){let r=t[n++];if(r>=0)i(r,e,t[n++]);else{let a=t[n+-r];for(let o=-r;o>0;o--)i(t[n++],e,a);n++}}}this.nodeSet=new io(t.map((t,i)=>ro.define({name:i>=this.minRepeatTerm?void 0:t,id:i,props:r[i],top:n.indexOf(i)>-1,error:i==0,skipped:e.skippedNodes&&e.skippedNodes.indexOf(i)>-1}))),e.propSources&&(this.nodeSet=this.nodeSet.extend(...e.propSources)),this.strict=!1,this.bufferLength=Qa;let a=Yo(e.tokenData);this.context=e.context,this.specializerSpecs=e.specialized||[],this.specialized=new Uint16Array(this.specializerSpecs.length);for(let e=0;e<this.specializerSpecs.length;e++)this.specialized[e]=this.specializerSpecs[e].term;this.specializers=this.specializerSpecs.map(_s),this.states=Yo(e.states,Uint32Array),this.data=Yo(e.stateData),this.goto=Yo(e.goto),this.maxTerm=e.maxTerm,this.tokenizers=e.tokenizers.map(e=>typeof e==`number`?new $o(a,e):e),this.topRules=e.topRules,this.dialects=e.dialects||{},this.dynamicPrecedences=e.dynamicPrecedences||null,this.tokenPrecTable=e.tokenPrec,this.termNames=e.termNames||null,this.maxNode=this.nodeSet.types.length-1,this.dialect=this.parseDialect(),this.top=this.topRules[Object.keys(this.topRules)[0]]}createParse(e,t,n){let r=new ls(this,e,t,n);for(let i of this.wrappers)r=i(r,e,t,n);return r}getGoto(e,t,n=!1){let r=this.goto;if(t>=r[0])return-1;for(let i=r[t+1];;){let t=r[i++],a=t&1,o=r[i++];if(a&&n)return o;for(let n=i+(t>>1);i<n;i++)if(r[i]==e)return o;if(a)return-1}}hasAction(e,t){let n=this.data;for(let r=0;r<2;r++)for(let i=this.stateSlot(e,r?2:1),a;;i+=3){if((a=n[i])==65535){if(n[i+1]==1)a=n[i=hs(n,i+2)];else if(n[i+1]==2)return hs(n,i+2);else break}if(a==t||a==0)return hs(n,i+1)}return 0}stateSlot(e,t){return this.states[e*6+t]}stateFlag(e,t){return(this.stateSlot(e,0)&t)>0}validAction(e,t){return!!this.allActions(e,e=>e==t||null)}allActions(e,t){let n=this.stateSlot(e,4),r=n?t(n):void 0;for(let n=this.stateSlot(e,1);r==null;n+=3){if(this.data[n]==65535){if(this.data[n+1]==1)n=hs(this.data,n+2);else break}r=t(hs(this.data,n+1))}return r}nextStates(e){let t=[];for(let n=this.stateSlot(e,1);;n+=3){if(this.data[n]==65535){if(this.data[n+1]==1)n=hs(this.data,n+2);else break}if(!(this.data[n+2]&1)){let e=this.data[n+1];t.some((t,n)=>n&1&&t==e)||t.push(this.data[n],e)}}return t}configure(t){let n=Object.assign(Object.create(e.prototype),this);if(t.props&&(n.nodeSet=this.nodeSet.extend(...t.props)),t.top){let e=this.topRules[t.top];if(!e)throw RangeError(`Invalid top rule name ${t.top}`);n.top=e}return t.tokenizers&&(n.tokenizers=this.tokenizers.map(e=>{let n=t.tokenizers.find(t=>t.from==e);return n?n.to:e})),t.specializers&&(n.specializers=this.specializers.slice(),n.specializerSpecs=this.specializerSpecs.map((e,r)=>{let i=t.specializers.find(t=>t.from==e.external);if(!i)return e;let a=Object.assign(Object.assign({},e),{external:i.to});return n.specializers[r]=_s(a),a})),t.contextTracker&&(n.context=t.contextTracker),t.dialect&&(n.dialect=this.parseDialect(t.dialect)),t.strict!=null&&(n.strict=t.strict),t.wrap&&(n.wrappers=n.wrappers.concat(t.wrap)),t.bufferLength!=null&&(n.bufferLength=t.bufferLength),n}hasWrappers(){return this.wrappers.length>0}getName(e){return this.termNames?this.termNames[e]:String(e<=this.maxNode&&this.nodeSet.types[e].name||e)}get eofTerm(){return this.maxNode+1}get topNode(){return this.nodeSet.types[this.top[1]]}dynamicPrecedence(e){let t=this.dynamicPrecedences;return t==null?0:t[e]||0}parseDialect(e){let t=Object.keys(this.dialects),n=t.map(()=>!1);if(e)for(let r of e.split(` `)){let e=t.indexOf(r);e>=0&&(n[e]=!0)}let r=null;for(let e=0;e<t.length;e++)if(!n[e])for(let n=this.dialects[t[e]],i;(i=this.data[n++])!=65535;)(r||=new Uint8Array(this.maxTerm+1))[i]=1;return new ds(e,n,r)}static deserialize(t){return new e(t)}};function hs(e,t){return e[t]|e[t+1]<<16}function gs(e){let t=null;for(let n of e){let e=n.p.stoppedAt;(n.pos==n.p.stream.end||e!=null&&n.pos>e)&&n.p.parser.stateFlag(n.state,2)&&(!t||t.score<n.score)&&(t=n)}return t}function _s(e){if(e.external){let t=+!!e.extend;return(n,r)=>e.external(n,r)<<1|t}return e.get}var vs=new ts(e=>{e.next===35&&e.peek(1)===40&&e.acceptToken(1,1)}),ys={__proto__:null,import:12,define:18,lambda:32,if:40,and:44,or:48,begin:52,cond:56,"??":64,let:70,match:82,export:88,"define-export":92,display:96,struct:100},bs=ms.deserialize({version:14,states:"4jQYQROOOOQP'#C`'#C`O!lQRO'#CiO!sQRO'#CjOOQP'#Ck'#CkOOQP'#Co'#CoOOQP'#Cq'#CqOOQP'#Cs'#CsOOQP'#Cu'#CuOOQP'#Cw'#CwOzQRO'#CzO!zQRO'#EPOOQP'#C}'#C}OOQP'#DO'#DOOOQP'#DU'#DUOOQP'#Dr'#DrOOQP'#Cd'#CdOOQP'#DX'#DXOOQP'#DZ'#DZOOQP'#D]'#D]OOQP'#D_'#D_OOQP'#Db'#DbOOQP'#Dm'#DmOOQP'#Dc'#DcQYQROOOOQP'#C{'#C{OOQP'#Dd'#DdO#}QRO,59TOOQP,59T,59TO$UQRO'#EPO$uQRO,59UOOQP,59U,59UOOQP,59f,59fO$|QRO,5:kOOQP,5:k,5:kO%TQQO,5:YO%]QQO,5:cOzQRO,5:fO%bQRO,5:gO%iQRO,5:hOzQRO,5:iO%pQQO,5:jO%xQQO,5:lOzQRO,5:pO%}QQO,5:]O&SQQO,5:qO&[QQO,5:rOzQRO,5:sO&aQQO,5:tOOQP-E7a-E7aOOQP-E7b-E7bOOQP1G.o1G.oOOQP1G.p1G.pOOQP1G0V1G0VO&fQQO1G/tO&nQQO'#DxOOQP'#Cm'#CmOzQRO1G/}OzQRO1G0QO&yQRO1G0ROOQP1G0R1G0RO'QQRO1G0SOOQP1G0S1G0SO'XQRO1G0TOzQRO'#CyOOQO'#Df'#DfO'`QQO1G0UOOQP1G0U1G0UO'hQQO'#EROOQP'#DQ'#DQOzQRO1G0WO'pQQO1G0[OzQRO1G/wOOQO'#De'#DeO'xQQO1G0]OOQP1G0]1G0]OzQRO1G0^O(QQQO1G0_O(VQQO1G0`OOQP7+%`7+%`O([QQO7+%`O(aQQO,5:dOOQP,5:d,5:dO(lQQO,5:eO(qQQO7+%iOzQRO7+%lOOQP7+%m7+%mOOQP7+%n7+%nO(vQRO7+%oOOQP7+%o7+%oOzQRO,59eOOQO-E7d-E7dOOQP7+%p7+%pO(}QQO'#DROOQO'#Dg'#DgO)fQQO,5:mOOQP,5:m,5:mO)nQQO7+%rO(}QQO'#DWOOQO'#Di'#DiO)sQQO7+%vOOQP7+%v7+%vO){QQO7+%cOOQO-E7c-E7cOOQP7+%w7+%wO*QQQO7+%xOOQP7+%y7+%yO*VQQO'#DxOOQO'#Da'#DaO*_QQO7+%zOOQP<<Hz<<HzOOQP1G0O1G0OO*dQQO1G0PO*iQQO1G0POOQP<<IT<<ITO*nQQO<<IWOOQP<<IZ<<IZO*sQQO1G/PO*xQQO'#ETOOQP'#DS'#DSO+PQQO'#DTOOQP'#ES'#ESOzQRO,59mOOQO-E7e-E7eOOQP1G0X1G0XOOQP<<I^<<I^OzQRO,59rOOQO-E7g-E7gOOQP<<Ib<<IbOOQP<<H}<<H}OOQP<<Id<<IdO+WQQO,5:dOOQP<<If<<IfO+`QQO7+%kOOQP7+%k7+%kOOQPAN>rAN>rOOQO7+$k7+$kOOQO'#Dh'#DhO+eQQO,5:oOOQP,5:o,5:oO+lQQO,59oOOQP,59o,59oO+sQQO1G/XO+xQQO1G/^OOQP<<IV<<IVOOQO-E7f-E7fOOQP1G0Z1G0ZOOQP1G/Z1G/ZOOQO7+$s7+$sOOQO7+$x7+$x",stateData:",T~O!`OSQOS~OPYOT_OV_OY_OZ_O[_OpiO!cZO!hQO!jRO~OPYOT_OV_OY_OZ_O[_OpiO!cmO!hQO!jRO~O!glO~PzO!ioO~PzOUsOX|O`tOduOfvOhwOjxOlyOszOy{O|}O!O!OO!Q!PO!S!QO!drO~PzO!g!TO~PzO`tOduOfvOhwOjxOlyOszOy{O!drO~PzO!i!UO~PzO!d!VO~PzOT!WOV!WO~O!c!XO~O!d!^O~PzO!d!`O~PzO!d!eO!h!bO~O!c!fO~OT!jO~OT!kO!d!mO~OT!nO~OT!pO~OT!rO!d!qO~OT!kOb!uO!d!tO~O!d!xO~PzO!d!yO~PzO!d!{O~PzO!d#OO!h!bO~O!d#SO!h#PO~O!d#XO!h#UO~OT!kO!d#[O~O!d#^O~O!c#_O~O!d#bO~OT!kOb#dO!d#cO~OT#eO~O!d#fO~O!d#hO~PzOT#mOV#mOY#mOZ#mO[#mO!c#jO!h#lO~O!d#pO!h#PO~O!d#qO~O!d#tO!h#UO~O!d#uO~O!d#vO~OT!kO!d!tO~O!d#xO~OT#yO~O!d#zO~O!d#{O~O!g#|O~O!d$PO~P(}O!g$RO~P(}OT!kO!d#cO~O!d$UO~O!d$WO~P(}O!g$XO~P(}O!g$YO~O!g$ZO~OYZ[T[~",goto:"/O!}PPPP#OPPP#OPPPP#S#S#SP#tP#SP#SP#SP#SP#SP#w#S#SP#S#SP#{$O$S$S#SP$[#OP#OP#OP#OP$`#O$c$i%U%d%j%p%zPPP&Q&UPP&Y&^PPPP'p(b(h(k)])}*o+a,R,s-e-h-t-|.n.r.v.zTfOh!__OQRYZhkmnquvwx{!P!Z![!]!_!a!b!h!j!n!w!z!|#n#rR!ZtT!cy!dR!hzT#Q!f#R]#m#P#U#j#l$O$QT#V!i#WR#a!pQhOR!RhQkQQnRSqZm[!Sknq!]!_!zQ!]vQ!_wR!z!aQ!l}Q!s!XU#Z!l!s#wR#w#_Q!dyR!}!dQ#R!fR#o#RQ$O#jQ$Q#lT$V$O$QQ#W!iR#s#WTgOhTPOhT`OhSeOhjjQRZkmnqvw!]!_!a!zQpYQ![uQ!axQ!i{Q!o!PQ!v!ZQ!w![Q!|!bQ#T!hQ#Y!jQ#]!nQ#g!wQ#i!|Q$S#nR$T#r!_SOQRYZhkmnquvwx{!P!Z![!]!_!a!b!h!j!n!w!z!|#n#rQ!YtR#`!pR!Yt!_TOQRYZhkmnquvwx{!P!Z![!]!_!a!b!h!j!n!w!z!|#n#r!_UOQRYZhkmnquvwx{!P!Z![!]!_!a!b!h!j!n!w!z!|#n#r!_VOQRYZhkmnquvwx{!P!Z![!]!_!a!b!h!j!n!w!z!|#n#r!_WOQRYZhkmnquvwx{!P!Z![!]!_!a!b!h!j!n!w!z!|#n#r!_XOQRYZhkmnquvwx{!P!Z![!]!_!a!b!h!j!n!w!z!|#n#r!_[OQRYZhkmnquvwx{!P!Z![!]!_!a!b!h!j!n!w!z!|#n#r!_]OQRYZhkmnquvwx{!P!Z![!]!_!a!b!h!j!n!w!z!|#n#rR!gzQ#n#PQ#r#UX#}#j#l$O$Q]#k#P#U#j#l$O$Q!_^OQRYZhkmnquvwx{!P!Z![!]!_!a!b!h!j!n!w!z!|#n#rTaOhTbOhTcOhTdOh",nodeNames:`⚠ AnonHash LineComment Program Import Identifier import String Define define Number Boolean Char Vector Obj Lambda lambda ArgList Amp If if And and Or or Begin begin Cond cond CondClause AnonFn Hole ?? Application Let let Bindings Binding PApp PVector Match match MatchClause Export export DefineExport define-export Display display Struct struct FieldList SExpr`,maxTerm:90,skippedNodes:[0,2],repeatNodeCount:7,tokenData:"De~RsOX#`XY'wYZ'wZ]#`]^'w^p#`pq'wqr#`rs(Yst)vtu#`uv2lvw3sxy3xyz3}z{#`{|4S|}#`}!O4S!O!PBh!P!Q#`!Q![;m![!]#`!]!^Ch!^!}#`!}#ODP#O#P#`#P#QDU#Q#o#`#o#pDZ#p#q#`#q#rD`#r;'S#`;'S;=`'q<%lO#`~#e_T~OX#`Z]#`^p#`qr#`sv#`z!O#`!O!P$d!P!]#`!^!}#`#O#P#`#Q#o#`#p#q#`#r;'S#`;'S;=`'q<%lO#`~$g_OX%fZ]%f^p%fqr%fsu%fuv&mz!O%f!P!]%f!^!}%f#O#P%f#Q#o%f#p#q%f#r;'S%f;'S;=`&g<%lO%f~%k^T~OX%fZ]%f^p%fqr%fsv%fz!O%f!P!]%f!^!}%f#O#P%f#Q#o%f#p#q%f#r;'S%f;'S;=`&g<%lO%f~&jP;=`<%l%f~&r_T~OX%fZ]%f^p%fqr%fsv%fvw%fz!O%f!P!]%f!^!}%f#O#P%f#Q#o%f#p#q%f#r;'S%f;'S;=`&g<%lO%f~'tP;=`<%l#`~'|S!`~XY'wYZ'w]^'wpq'w~(]VOr(Yrs(rs#O(Y#O#P(w#P;'S(Y;'S;=`)p<%lO(Y~(wOV~~(zRO;'S(Y;'S;=`)T;=`O(Y~)WWOr(Yrs(rs#O(Y#O#P(w#P;'S(Y;'S;=`)p;=`<%l(Y<%lO(Y~)sP;=`<%l(Y~){cT~OX#`Z]#`^p#`qr#`sv#`z!O#`!O!P$d!P!]#`!^!}#`#O#P+W#Q#Y#`#Y#Z1f#Z#h#`#h#i1f#i#o#`#p#q#`#r;'S#`;'S;=`'q<%lO#`~+]mT~OX-WZ]-W^p-Wqr-Wrs.^sv-Wvx.^z{.c{|.c|}-W}!O.c!O!P0[!P!Q-W!Q![.c![!]-W!]!^.^!^!_-W!_!`.c!`!c-W!c!}.c#O#P-W#Q#R-W#R#S.c#S#T-W#T#o.c#p#q-W#r;'S-W;'S;=`1`<%lO-W~-__[~T~OX#`Z]#`^p#`qr#`sv#`z!O#`!O!P$d!P!]#`!^!}#`#O#P#`#Q#o#`#p#q#`#r;'S#`;'S;=`'q<%lO#`~.cO[~~.jj[~T~OX#`Z]#`^p#`qr#`sv#`z{.c{|.c|}#`}!O.c!O!P$d!P!Q#`!Q![.c![!]#`!^!_#`!_!`.c!`!c#`!c!}.c#O#P#`#Q#R#`#R#S.c#S#T#`#T#o.c#p#q#`#r;'S#`;'S;=`'q<%lO#`~0a_[~OX%fZ]%f^p%fqr%fsu%fuv&mz!O%f!P!]%f!^!}%f#O#P%f#Q#o%f#p#q%f#r;'S%f;'S;=`&g<%lO%f~1cP;=`<%l-W~1m_Z~T~OX#`Z]#`^p#`qr#`sv#`z!O#`!O!P$d!P!]#`!^!}#`#O#P#`#Q#o#`#p#q#`#r;'S#`;'S;=`'q<%lO#`~2q`T~OX#`Z]#`^p#`qr#`sv#`vw#`z!O#`!O!P$d!P!]#`!^!}#`#O#P#`#Q#o#`#p#q#`#r;'S#`;'S;=`'q<%lO#`~3xOb~~3}O!c~~4SO!d~~4XaT~OX#`Z]#`^p#`qr#`sv#`z!O#`!O!P5^!P!Q#`!Q![;m![!]#`!^!}#`#O#P#`#Q#o#`#p#q#`#r;'S#`;'S;=`'q<%lO#`~5aaOX%fZ]%f^p%fqr%fsu%fuv&mz!O%f!P!Q%f!Q![6f![!]%f!^!}%f#O#P%f#Q#o%f#p#q%f#r;'S%f;'S;=`&g<%lO%f~6mdY~T~OX%fZ]%f^p%fqr%fsv%fz!O%f!P!Q%f!Q![6f![!]%f!^!g%f!g!h7{!h!}%f#O#P%f#Q#X%f#X#Y7{#Y#o%f#p#q%f#r;'S%f;'S;=`&g<%lO%f~8QcT~OX%fZ]%f^p%fqr%fsv%fz{%f{|9]|}%f}!O9]!P!Q%f!Q![:d![!]%f!^!}%f#O#P%f#Q#o%f#p#q%f#r;'S%f;'S;=`&g<%lO%f~9b`T~OX%fZ]%f^p%fqr%fsv%fz!O%f!P!Q%f!Q![:d![!]%f!^!}%f#O#P%f#Q#o%f#p#q%f#r;'S%f;'S;=`&g<%lO%f~:k`Y~T~OX%fZ]%f^p%fqr%fsv%fz!O%f!P!Q%f!Q![:d![!]%f!^!}%f#O#P%f#Q#o%f#p#q%f#r;'S%f;'S;=`&g<%lO%f~;teY~T~OX#`Z]#`^p#`qr#`sv#`z!O#`!O!P=V!P!Q#`!Q![;m![!]#`!^!g#`!g!h>m!h!}#`#O#P#`#Q#X#`#X#Y>m#Y#o#`#p#q#`#r;'S#`;'S;=`'q<%lO#`~=[eY~OX%fZ]%f^p%fqr%fsu%fuv&mz!O%f!P!Q%f!Q![6f![!]%f!^!g%f!g!h7{!h!}%f#O#P%f#Q#X%f#X#Y7{#Y#o%f#p#q%f#r;'S%f;'S;=`&g<%lO%f~>rdT~OX#`Z]#`^p#`qr#`sv#`z{#`{|@Q|}#`}!O@Q!O!P$d!P!Q#`!Q![A[![!]#`!^!}#`#O#P#`#Q#o#`#p#q#`#r;'S#`;'S;=`'q<%lO#`~@VaT~OX#`Z]#`^p#`qr#`sv#`z!O#`!O!P$d!P!Q#`!Q![A[![!]#`!^!}#`#O#P#`#Q#o#`#p#q#`#r;'S#`;'S;=`'q<%lO#`~AcaY~T~OX#`Z]#`^p#`qr#`sv#`z!O#`!O!P$d!P!Q#`!Q![A[![!]#`!^!}#`#O#P#`#Q#o#`#p#q#`#r;'S#`;'S;=`'q<%lO#`~BkP!Q![Bn~BsRY~!Q![Bn!g!hB|#X#YB|~CPR{|CY}!OCY!Q![C`~C]P!Q![C`~CePY~!Q![C`~CmSQ~OYChZ;'SCh;'S;=`Cy<%lOCh~C|P;=`<%lCh~DUO!h~~DZO!g~~D`O!j~~DeO!i~",tokenizers:[vs,0],topRules:{Program:[0,3]},specialized:[{term:5,get:e=>ys[e]||-1}],tokenPrec:505});function xs(e){let t=[0];for(let n=0;n<e.length;n++)e[n]===`
`&&t.push(n+1);return t}function Ss(e,t){let n=0,r=t.length-1;for(;n<r;){let i=n+r+1>>1;t[i]<=e?n=i:r=i-1}return new Ir(n+1,e-t[n]+1,e)}var Cs=class{src;lineStarts;diagnostics;allowInternalNames;anonFnDepth=0;constructor(e,t,n,r=!1){this.src=e,this.lineStarts=t,this.diagnostics=n,this.allowInternalNames=r}range(e){return new Yr(Ss(e.from,this.lineStarts),Ss(Math.max(e.from,e.to-1),this.lineStarts))}text(e){return this.src.slice(e.from,e.to)}};function ws(e){let t=[],n=e.firstChild;for(;n;)n.type.name!==`LineComment`&&t.push(n),n=n.nextSibling;return t}var Ts=new Set([`ArgList`,`FieldList`,`Bindings`,`Binding`,`CondClause`,`MatchClause`]);function Es(e){return e.flatMap(e=>Ts.has(e.type.name)?[e,...Es(ws(e))]:[e])}var Ds={Lambda:`lambda expression (a list of parameters and a body)`,If:`if expression (a guard, an if-branch, and an else-branch)`,Let:`let expression (a list of bindings and a body)`,Cond:`cond expression (a list of [test body] branches)`,Match:`match expression (a scrutinee and a list of [pattern body] branches)`,And:`and expression`,Or:`or expression`,Begin:`begin expression (at least one sub-expression)`,AnonFn:`anonymous function #(...)`,Application:`function application`,Vector:`vector literal`,Obj:`map literal (an even number of key/value expressions)`,PApp:`constructor pattern`,PVector:`vector pattern`,Import:`import statement (a built-in library name, or a quoted file name)`,Define:`define statement (a name and a value)`,Export:`export statement (a list of names to export)`,DefineExport:`define-export statement (a name and a value)`,Display:`display statement (a value to display)`,Struct:`struct statement (a name and a list of fields)`};function Os(e,t){if(t.type.isError){e.diagnostics.push(J(`Parse`,`error`,`Malformed syntax.`,e.range(t)));return}let n=Ds[t.type.name]??`${t.type.name.toLowerCase()} expression`;e.diagnostics.push(J(`Parse`,`error`,`Malformed ${n}.`,e.range(t)))}function ks(e,t,n,r){if(t.type.isError||n.some(e=>e.type.isError))return Os(e,t),r}function As(e,t){let n=e.text(t);switch(t.type.name){case`Number`:return ra(n);case`String`:return ia(n,e.range(t));case`Boolean`:return n===`#t`;case`Char`:return aa(n,e.range(t));default:throw new H(`lezer-bridge.leafValue`,`Unexpected leaf node: ${t.type.name}`)}}var js=(e,t)=>t.type.name===`Identifier`&&e.text(t)===`null`;function Ms(e){return e===`%`||e===`%&`||/^%[1-9][0-9]*$/.test(e)}function Ns(e){return e.length>4&&e.startsWith(`##`)&&e.endsWith(`##`)}function Ps(e,t,n){let{qualifier:r,member:i}=et(n);for(let a of[r,i])if(sa.includes(a)||a.startsWith(`%`))return e.diagnostics.push(J(`Parse`,`error`,`The qualified name "${n}" is invalid: "${a}" is not a valid name`,e.range(t))),`<error>`;return n}function Fs(e,t,n=`Expected an identifier`,r=!1,i=!1){let a=e.text(t);if(sa.includes(a))return e.diagnostics.push(J(`Parse`,`error`,`The identifier "${a}" is a reserved word and cannot be used as a variable name`,e.range(t))),`<error>`;if(t.type.name!==`Identifier`)return e.diagnostics.push(J(`Parse`,`error`,n,e.range(t))),`<error>`;if(qt(a))return i?Ps(e,t,a):(e.diagnostics.push(J(`Parse`,`error`,`Qualified names (like "${a}") may only be used as variable references, not as a binding name`,e.range(t))),`<error>`);if(Ns(a)&&!r&&!e.allowInternalNames)return e.diagnostics.push(J(`Parse`,`error`,`The identifier "${a}" is reserved for Scamper's internal use and cannot be used as a binding name`,e.range(t))),`<error>`;if(a.startsWith(`%`)){if(!Ms(a))return e.diagnostics.push(J(`Parse`,`error`,`The identifier "${a}" is invalid: identifiers cannot begin with "%" (only "%", "%1", ..., "%k", and "%&" may, and only inside an anonymous function #(...))`,e.range(t))),`<error>`;if(!r)return e.diagnostics.push(J(`Parse`,`error`,`The identifier "${a}" cannot be used as a binding name`,e.range(t))),`<error>`;if(e.anonFnDepth===0)return e.diagnostics.push(J(`Parse`,`error`,`The identifier "${a}" can only be used inside an anonymous function #(...)`,e.range(t))),`<error>`}return a}function Is(e,t,n=`Expected an identifier`,r=!1,i=!1){return N(Fs(e,t,n,r,i),e.range(t))}function Ls(e,t){let n=[],r=e.range(t).begin.line-1,i=t.prevSibling;for(;i?.type.name===`LineComment`;){let t=e.range(i);if(t.begin.line!==r)break;let a=i.prevSibling;if(a!==null&&e.range(a).end.line===t.begin.line)break;n.unshift({line:e.text(i),range:t}),r=t.begin.line-1,i=a}return n.length>0?n:void 0}function Rs(e,t){let n=e.range(t),r=ws(t),i=ks(e,t,r,pe(`<error>`,n));if(i)return i;switch(t.type.name){case`Number`:case`String`:case`Boolean`:case`Char`:return pe(As(e,t),n);case`Identifier`:{if(js(e,t))return pe(null,n);let r=Is(e,t,`Expected a valid constructor name`);return r.name===`_`?At(n):r}case`PApp`:{if(r.length===0)return pe(null,n);let t=Is(e,r[0],`The first element of a pattern list must be a constructor name`),i=r.slice(1).map(t=>Rs(e,t));return Tt(t,i,n)}case`PVector`:return bt(r.map(t=>Rs(e,t)),n);default:throw new H(`lezer-bridge.patFromNode`,`Unexpected pattern node: ${t.type.name}`)}}function $(e,t){let n=e.range(t),r=ws(t),i=ks(e,t,Es(r),M(void 0,n));if(i)return i;switch(t.type.name){case`Number`:case`String`:case`Boolean`:case`Char`:return M(As(e,t),n);case`Identifier`:return js(e,t)?M(null,n):Is(e,t,`Expected an identifier`,!0,!0);case`Hole`:return zn(n);case`Vector`:return hr(r.map(t=>$(e,t)),n);case`Obj`:return r.length%2==0?ni(pa(r.map(t=>$(e,t))).map(([e,t])=>({key:e,value:t})),n):(e.diagnostics.push(J(`Parse`,`error`,`A map literal must have an even number of expressions (alternating keys and values), but this one has ${r.length.toString()}`,n)),M(void 0,n));case`Lambda`:{let[t,i]=r.slice(1),a=$(e,i),o=ws(t),s=o.findIndex(e=>e.type.name===`Amp`);if(s===-1){let t=o.map(t=>Is(e,t));return ie(t,a,n)}let c=o.slice(0,s).map(t=>Is(e,t)),l=Is(e,o[s+1]);return ie(c,a,n,l)}case`If`:{let t=r.slice(1);return P($(e,t[0]),$(e,t[1]),$(e,t[2]),n)}case`And`:return ut(r.slice(1).map(t=>$(e,t)),n);case`Or`:return h(r.slice(1).map(t=>$(e,t)),n);case`Begin`:return ht(r.slice(1).map(t=>$(e,t)),n);case`AnonFn`:{e.anonFnDepth>0&&e.diagnostics.push(J(`Parse`,`error`,`Anonymous functions #(...) cannot be nested`,n)),e.anonFnDepth++;let t=$(e,r[1]);return e.anonFnDepth--,b(t,n)}case`Application`:return r.length===0?M(null,n):U($(e,r[0]),r.slice(1).map(t=>$(e,t)),n);case`Let`:{let[t,i]=r.slice(1),a=$(e,i),o=ws(t).map(t=>{let[n,r]=ws(t);return{pat:Rs(e,n),value:$(e,r)}});return ve(o,a,n)}case`Cond`:{let t=r.slice(1).map(t=>{let[n,r]=ws(t);return{test:$(e,n),body:$(e,r)}});return O(t,n)}case`Match`:{let t=r.slice(1),i=$(e,t[0]),a=t.slice(1).map(t=>{let[n,r]=ws(t);return{pat:Rs(e,n),body:$(e,r)}});return at(i,a,n)}default:throw new H(`lezer-bridge.expFromNode`,`Unexpected expression node: ${t.type.name}`)}}function zs(e,t){let n=e.range(t),r=ws(t),i=ks(e,t,r,ce(M(void 0,n),n));if(i)return i;switch(t.type.name){case`Import`:{let t=r[1],i=r.length>2?Fs(e,r[2],`Expected a module alias`):void 0;if(t.type.name===`String`){let r=As(e,t);return Ye(r,`file`,n,i)}let a=e.text(t);if(t.type.name===`Identifier`&&qt(a))return e.diagnostics.push(J(`Parse`,`error`,`Malformed import statement: a file name like "${a}" must be quoted, e.g. (import "${a}")`,e.range(t))),ce(M(void 0,n),n);let o=Fs(e,t);return Ye(o,`builtin`,n,i)}case`Define`:{let i=r.slice(1),a=Is(e,i[0]),o=$(e,i[1]),s=Ls(e,t);return T(a,o,n,s)}case`Export`:{let t=r.slice(1).map(t=>Is(e,t));return u(t,n)}case`DefineExport`:{let i=r.slice(1),a=Is(e,i[0]),o=$(e,i[1]),s=Ls(e,t);return We(a,o,n,s)}case`Display`:{let t=$(e,r[1]);return Un(t,n)}case`Struct`:{let[t,i]=r.slice(1),a=Is(e,t),o=ws(i).map(t=>Is(e,t));return De(a,o,n)}case`SExpr`:return ce($(e,r[0]),n);default:throw new H(`lezer-bridge.stmtFromNode`,`Unexpected statement node: ${t.type.name}`)}}function Bs(e,t,n={}){let r=bs.parse(t),i=new Cs(t,xs(t),e,n.allowInternalNames),a=[];for(let e of ws(r.topNode)){if(e.type.isError){Os(i,e);continue}a.push(zs(i,e))}return a}var Vs;function Hs(){if(!Vs)throw Error(`SymbolDB used before initialize()`);return Vs}function Us(){Vs||(Vs=new Map,am.forEach((e,t)=>{Ws(t,e)}))}function Ws(e,t){let n=[];t.bindings.forEach((e,t)=>{n.push(N(t))}),Hs().set(e,n)}async function Gs(e){let{getFS:t}=await ii(async()=>{let{getFS:e}=await import(`./fs-43.4.0-rc.1.js`);return{getFS:e}},__vite__mapDeps([0,1,2,3])),n=await t().loadFile(e),r=[],i=Bs(r,n);if(r.length>0)throw ea(r[0]);return i}function Ks(e){let t=new Map,n=new Set,r=e=>t.set(e.name,e);for(let t of e)switch(t.tag){case`define`:r(t.name);break;case`defexport`:r(t.name),n.add(t.name.name);break;case`struct`:r(t.name),r(N(`${t.name.name}?`,t.name.range));for(let e of t.fields)r(N(`${t.name.name}-${e.name}`,t.name.range));break;case`export`:for(let e of t.names)n.add(e.name)}let i=[];for(let e of n){let n=t.get(e);n!==void 0&&i.push(n)}return i}function qs(e){return e.filter(e=>e.tag===`import`&&e.kind===`file`)}async function Js(e){let t=[],n=new Set,r=async(e,i,a)=>{for(let o of qs(e)){let e=o.module;if(!n.has(e)){n.add(e);try{let t=await Gs(e);Hs().set(e,Ks(t)),await r(t,e,a??o)}catch{i!==void 0&&a!==void 0&&t.push({filename:e,importer:i,range:a.range})}}}};return await r(e,void 0,void 0),t}function Ys(e){return Hs().get(e)}function Xs(e,t,n){let r=new Set;for(let i of t)r.has(i)&&e.push(J(`Scope`,`warning`,`Duplicate variable '${i}' encountered in binding list`,n)),r.add(i)}function Zs(e,t,n){switch(n.tag){case`id`:t.has(n.name)?e.push(J(`Scope`,`warning`,`Duplicate binding variable '${n.name}' encountered in pattern`,n.range)):t.add(n.name);return;case`pwild`:return;case`plit`:return;case`pctor`:case`pvec`:n.args.forEach(n=>{Zs(e,t,n)});return}}function Qs(e,t,n,r,i){switch(i.tag){case`id`:if(qt(i.name)){let{qualifier:t,member:r}=et(i.name),a=n.get(t);a===void 0?e.push(J(`Scope`,`warning`,`No imported module is qualified as '${t}'`,i.range)):a.exports.has(r)||e.push(J(`Scope`,`warning`,`Module '${t}' (${a.module}) has no exported binding '${r}'`,i.range));return}!r.includes(i.name)&&!t.includes(i.name)&&e.push(J(`Scope`,`warning`,`Undefined variable '${i.name}'`,i.range));return;case`lit`:case`hole`:return;case`app`:Qs(e,t,n,r,i.head),i.args.forEach(i=>{Qs(e,t,n,r,i)});return;case`lam`:{let a=(i.restParam?[...i.params,i.restParam]:i.params).map(e=>e.name);Xs(e,a,i.range),Qs(e,t,n,[...r,...a],i.body);return}case`let`:{let a=new Set;i.bindings.forEach(t=>{let n=new Set;Zs(e,n,t.pat);for(let t of n)a.has(t)&&e.push(J(`Scope`,`error`,`Duplicate binding '${t}' in let`,i.range)),a.add(t)});let o=[...r,...a];i.bindings.forEach(r=>{Qs(e,t,n,o,r.value)}),Qs(e,t,n,o,i.body);return}case`if`:Qs(e,t,n,r,i.guard),Qs(e,t,n,r,i.ifB),Qs(e,t,n,r,i.elseB);return;case`match`:Qs(e,t,n,r,i.scrutinee),i.branches.forEach(i=>{let a=new Set;Zs(e,a,i.pat),Qs(e,t,n,[...r,...a],i.body)});return;default:throw new H(`scopeCheckExp`,`Non-core expression encountered ${i.tag}`)}}function $s(e,t,n){Zr(t)?n.includes(t.name)||e.push(J(`Docstring`,`warning`,`Undefined predicate "${t.name}"`,t.range)):ec(e,t,n)}function ec(e,{head:{name:t},args:n,range:r},i){i.includes(t)||e.push(J(`Docstring`,`warning`,`Undefined predicate "${t}"`,r));for(let t of n)$s(e,t,i)}function tc(e,t,{name:n,value:r},i){let a=n.name,{signature:{function:{head:{name:o},args:s},predicate:c,range:l,isConstant:u},params:d,range:f}=t;if(a!==o&&e.push(J(`Docstring`,`warning`,`Docstring name "${o}" does not match defined name "${a}"`,l)),u){$s(e,c,i);return}if(!kn(r)){e.push(J(`Docstring`,`warning`,`Function docstring attached to non-function definition. A constant is documented "name: predicate".`,t.range));return}let p=r.params.map(e=>e.name),m=[...s.map(e=>e.name)];for(let t of p){let n=m.shift();if(n===void 0){e.push(J(`Docstring`,`warning`,`Expected function parameter "${t}" to be defined in docstring signature`,l));continue}t!==n&&e.push(J(`Docstring`,`warning`,`Function signature defines parameter "${t}" in this position but docstring signature instead defines "${n}"`,l))}$s(e,c,i);let h=new Map([...p].map(e=>[e,!1]));for(let{name:t,predicate:n,range:r}of d)p.includes(t)||e.push(J(`Docstring`,`warning`,`Docstring describes unknown function parameter "${t}"`,r)),h.set(t,!0),$s(e,n,i);for(let[t,n]of h)n||e.push(J(`Docstring`,`warning`,`Description of function parameter "${t}" missing`,f))}async function nc(e,t){if(t.kind===`builtin`){let n=Ys(t.module);return n===void 0&&e.push(J(`Scope`,`warning`,`No such built-in library: '${t.module}'`,t.range)),n}let{getFS:n}=await ii(async()=>{let{getFS:e}=await import(`./fs-43.4.0-rc.1.js`);return{getFS:e}},__vite__mapDeps([0,1,2,3])),r;try{r=await n().fileExists(t.module)}catch(n){e.push(J(`Scope`,`warning`,n instanceof Error?n.message:String(n),t.range));return}if(!r){e.push(J(`Scope`,`warning`,`File '${t.module}' does not exist`,t.range));return}let i=Ys(t.module);return i===void 0&&e.push(J(`Scope`,`warning`,`Could not load module '${t.module}'`,t.range)),i}async function rc(e,t,n,r,i){switch(i.tag){case`import`:{let a=await nc(e,i);if(a===void 0)return;if(i.alias!==void 0){let t=n.get(i.alias);t!==void 0&&t.module!==i.module?e.push(J(`Scope`,`warning`,`Qualified name '${i.alias}' is already bound to module '${t.module}'`,i.range)):t===void 0&&n.set(i.alias,{module:i.module,exports:new Set(a.map(e=>e.name))});return}for(let{name:n}of a){let a=r.get(n);a!==void 0&&a!==i.module?e.push(J(`Scope`,`warning`,`Global variable '${n}' is already defined`,i.range)):a===void 0&&(t.includes(n)||t.push(n),r.set(n,i.module))}return}case`define`:{let n=i.name.name;t.includes(n)?e.push(J(`Scope`,`warning`,`Global variable '${n}' is already defined`,i.range)):t.push(n),r.set(n,null);return}case`export`:case`display`:case`stmtexp`:return;default:throw new H(`collectTopLevelBindings`,`Non-core statement encountered ${i.tag}`)}}function ic(e,t,n,r,i){switch(i.tag){case`import`:return;case`export`:for(let n of i.names)r.get(n.name)!==null&&e.push(J(`Scope`,`warning`,t.includes(n.name)?`Cannot export '${n.name}': it is not defined in this module`:`Exporting undefined variable '${n.name}'`,n.range));return;case`define`:if(Qs(e,t,n,[],i.value),i.docComments){let{doc:n,diagnostics:r}=Za(i.docComments);e.push(...r),n&&tc(e,n,i,t)}return;case`display`:Qs(e,t,n,[],i.value);return;case`stmtexp`:Qs(e,t,n,[],i.expr);return;default:throw new H(`scopeCheckStmtBodies`,`Non-core statement encountered ${i.tag}`)}}async function ac(e,t){for(let n of await Js(t))e.push(J(`Scope`,`warning`,`Could not load module '${n.filename}' (imported by '${n.importer}')`,n.range));let n=[];for(let e of Ys(`runtime`))n.push(e.name);for(let e of Ys(`prelude`))n.push(e.name);let r=new Map,i=new Map;for(let a of t)await rc(e,n,i,r,a);for(let a of t)ic(e,n,i,r,a)}function oc(e){switch(e.tag){case`pwild`:return At(e.range);case`plit`:return pe(e.value,e.range);case`pvar`:return N(e.name,e.range);case`pctor`:return Tt(N(e.name,e.range),e.args.map(oc),e.range);case`pvec`:return bt(e.args.map(oc),e.range)}}function sc(e){return e.map(e=>(v(e)||ct(e))&&e.name?N(e.name):M(e))}function cc(e,t,n){for(let r=n.length-1;r>=0;r--){let i=n[r];switch(i.tag){case`lit`:e.push(M(i.value,i.range,i.provenance));break;case`var`:{let n=t.lookup(i.name);n.found&&n.slot!==En&&!v(n.slot)?e.push(M(n.slot)):e.push(N(i.name));break}case`cls`:{let n=i.restParam?[...i.params,i.restParam]:i.params,r=cc([],t.withoutLocals(...n),i.body.toReversed());i.provenance===`anon-fn`?e.push(ie(i.params.map(e=>N(e)),r,i.range,i.restParam?N(i.restParam):void 0,`anon-fn`)):i.name?e.push(N(i.name)):e.push(ie(i.params.map(e=>N(e)),r,void 0,i.restParam?N(i.restParam):void 0));break}case`ap`:{let t=e.splice(-(i.numArgs+1)),n=t[0],r=i.numArgs===0?[]:t.slice(1);e.push(U(n,r,i.range,i.provenance));break}case`match`:{let n=Gn(e,`the raise stack`),r=i.branches.map(([e,n])=>{let r=cc([],t.withoutLocals(...on(e)),n.toReversed());return{pat:oc(e),body:r}});e.push(at(n,r));break}case`let`:if(i.idx===0){let n=t.withoutLocals(...i.bindings.flatMap(e=>on(e.pat))),r=i.bindings.map(e=>({pat:oc(e.pat),value:cc([],n,e.value.toReversed())}));e.push(ve(r,cc([],n,i.body.toReversed()),i.range,i.provenance))}else{let n=Gn(e,`the raise stack`),r=i.bindings.slice(i.idx-1),a=t.withoutLocals(...r.flatMap(e=>on(e.pat))),o=r.map((e,t)=>({pat:oc(e.pat),value:t===0?n:cc([],a,e.value.toReversed())}));e.push(ve(o,cc([],a,i.body.toReversed()),i.range,i.provenance))}break;case`if`:{let n=Gn(e,`the raise stack`),r=cc([],t,i.thenB.toReversed()),a=cc([],t,i.elseB.toReversed());e.push(P(n,r,a,i.range,i.provenance));break}case`hole`:e.push(zn(i.range));break;case`pop-scope`:break;case`ap-spread`:{let[t,n]=e.splice(-2);e.push(U(N(`apply`),[t,n]));break}case`push-handler`:break;case`pop-handler`:{let t=Gn(e,`the raise stack`);e.pop(),e.push(t);break}}}return Gn(e,`the raise stack`)}function lc(e){if(e.length===0)throw new H(`raiseFrames`,`no frames to raise`);let t=e[e.length-1],n=cc(sc(t.values),t.env,t.ops);for(let t=e.length-2;t>=0;t--){let r=sc(e[t].values);r.push(n),n=cc(r,e[t].env,e[t].ops)}return n}function uc(e){return lc(e.frames)}function dc(e,t){let n=e.findIndex(e=>e.range.contains(t));if(n<0)return{ok:!1,diagnostic:J(`Query`,`error`,`Received invalid query location: ${t.toString()}`)};let{stmt:r,range:i}=fc(e[n],t);return{ok:!0,prog:e.map((e,t)=>t===n?r:e),range:i}}function fc(e,t){switch(e.tag){case`define`:{let n=mc(e.value,t);return{stmt:T(e.name,n.exp,e.range,e.docComments),range:n.range}}case`defexport`:{let n=mc(e.value,t);return{stmt:We(e.name,n.exp,e.range,e.docComments),range:n.range}}case`display`:{let n=mc(e.value,t);return{stmt:Un(n.exp,e.range),range:n.range}}case`stmtexp`:{let n=mc(e.expr,t);return{stmt:ce(n.exp,e.range),range:n.range}}case`import`:case`struct`:case`export`:return{stmt:e,range:e.range}}}function pc(e){switch(e.tag){case`lit`:case`id`:case`hole`:return[];case`app`:return[{exp:e.head,rebuild:t=>U(t,e.args,e.range)},...e.args.map((t,n)=>({exp:t,rebuild:t=>U(e.head,e.args.map((e,r)=>r===n?t:e),e.range)}))];case`lam`:return[{exp:e.body,rebuild:t=>ie(e.params,t,e.range,e.restParam)}];case`if`:return[{exp:e.guard,rebuild:t=>P(t,e.ifB,e.elseB,e.range)},{exp:e.ifB,rebuild:t=>P(e.guard,t,e.elseB,e.range)},{exp:e.elseB,rebuild:t=>P(e.guard,e.ifB,t,e.range)}];case`and`:case`or`:case`begin`:{let t={and:ut,or:h,begin:ht}[e.tag];return e.exps.map((n,r)=>({exp:n,rebuild:n=>t(e.exps.map((e,t)=>t===r?n:e),e.range)}))}case`anonfn`:return[{exp:e.body,rebuild:t=>b(t,e.range)}];case`let`:return[...e.bindings.map((t,n)=>({exp:t.value,rebuild:t=>ve(e.bindings.map((e,r)=>r===n?{pat:e.pat,value:t}:e),e.body,e.range)})),{exp:e.body,rebuild:t=>ve(e.bindings,t,e.range)}];case`cond`:return e.branches.flatMap((t,n)=>[{exp:t.test,rebuild:t=>O(e.branches.map((e,r)=>r===n?{test:t,body:e.body}:e),e.range)},{exp:t.body,rebuild:t=>O(e.branches.map((e,r)=>r===n?{test:e.test,body:t}:e),e.range)}]);case`match`:return[{exp:e.scrutinee,rebuild:t=>at(t,e.branches,e.range)},...e.branches.map((t,n)=>({exp:t.body,rebuild:t=>at(e.scrutinee,e.branches.map((e,r)=>r===n?{pat:e.pat,body:t}:e),e.range)}))];case`vec`:return e.exps.map((t,n)=>({exp:t,rebuild:t=>hr(e.exps.map((e,r)=>r===n?t:e),e.range)}));case`obj`:return e.pairs.flatMap((t,n)=>[{exp:t.key,rebuild:t=>ni(e.pairs.map((e,r)=>r===n?{key:t,value:e.value}:e),e.range)},{exp:t.value,rebuild:t=>ni(e.pairs.map((e,r)=>r===n?{key:e.key,value:t}:e),e.range)}])}}function mc(e,t){for(let n of pc(e))if(n.exp.range.contains(t)){let e=mc(n.exp,t);return{exp:n.rebuild(e.exp),range:e.range}}return{exp:U(N(`##report##`,e.range),[e],e.range),range:e.range}}function hc(e){let t=[];for(let n of e){if(n.tag!==`define`&&n.tag!==`defexport`||n.docComments===void 0)continue;let{doc:e}=Za(n.docComments);if(e!==void 0)for(let n of e.tags)ka(n)&&t.push({range:n.range,call:n.contents.functionCall,expected:n.contents.result})}return t}function gc(e,t){let n=hr([t.call,t.expected]);return[...e,ce(U(N(`##report##`),[n]))]}function _c(e){let t=e.at(0);if(t===void 0||!(t instanceof fn))return{status:`error`,message:t?.message??`The example never produced a value.`};let n=t.value;if(!nn(n)||n.length!==2)return{status:`error`,message:`The example never produced a value.`};let[r,i]=n;return Gt(r,i)?{status:`pass`}:{status:`fail`,actual:r,expected:i}}var vc=`##contract-target##`,yc=`##contracted##`;function bc(e){return e.name.endsWith(`?`)?e.name.slice(0,-1):e.name}function xc(e){if(e.tag!==`id`){let t=e.args;if(e.head.name===`or/p`&&t.length>0&&t.every(Zr)){let e=t.map(bc);return e.length===1?e[0]:e.length===2?`${e[0]} or ${e[1]}`:`${e.slice(0,-1).join(`, `)}, or ${e[e.length-1]}`}return`a value matching \`${xn(e)}\``}let t=bc(e);return`${/^[aeiou]/i.test(t)?`an`:`a`} ${t}`}function Sc(e,t,n){return U(N(`string-append`,n),[M(`expected `,n),M(e,n),M(`, received `,n),U(N(`##typeOf##`,n),[N(t,n)],n)],n)}function Cc(e,t,n){return U(N(`string-append`,n),[M(`expected every value of ${t} to be `,n),M(e,n),M(`, but at least one was not`,n)],n)}function wc(e,t,n){if(!t)return U(N(vc,n),e.map(e=>N(e.name,n)),n);let r=e.reduceRight((e,t)=>U(N(`cons`,n),[N(t.name,n),e],n),N(t.name,n));return U(N(`##ap-spread##`,n),[N(vc,n),r],n)}function Tc(e,t,n,r){let i=wc([...e,...t],n,r),a=n?P(U(N(`all-satisfy?`,r),[n.predicate,N(n.name,r)],r),i,U(N(`##error##`,r),[Cc(xc(n.predicate),n.name,r)],r),r):i,o=t.reduceRight((e,{name:t,predicate:n})=>i=>P(P(U(N(Ac,r),[N(t,r)],r),M(!0,r),U(n,[N(t,r)],r),r),e(i),U(N(`##error##`,r),[Sc(xc(n),t,r)],r),r),e=>e),s=t=>{if(t===e.length)return o(a);let{name:n,predicate:i}=e[t];return P(U(i,[N(n,r)],r),s(t+1),U(N(`##error##`,r),[Sc(xc(i),n,r)],r),r)};return s(0)}var Ec=`##opts##`,Dc=`##optArg##`,Oc=`##optRest##`,kc=`##checkArity##`,Ac=`##voidQ##`;function jc(e,t,n,r,i){let a=()=>N(Ec,i),o=t.length,s=n?ve([{pat:N(n.name,i),value:U(N(Oc,i),[a(),M(o,i)],i)}],r,i):ht([U(N(kc,i),[a(),M(o,i),M(e,i)],i),r],i);for(let e=o-1;e>=0;e--)s=ve([{pat:N(t[e].name,i),value:U(N(Dc,i),[a(),M(e,i)],i)}],s,i);return s}function Mc(e){return e.params.length>0||e.optParams.length>0||e.restParam!==void 0}function Nc(e,t){if(t.tag!==`define`&&t.tag!==`defexport`||!t.docComments)return t;let{doc:n}=Za(t.docComments);if(!n)return t;let r=n.signature.function.head.name;if(r!==t.name.name)return e.push(J(`Docstring`,`error`,`Docstring signature names "${r}", but the definition below it is "${t.name.name}". A docstring is attached to the definition directly beneath it: move that definition above the docstring block, or correct the name in the signature.`,n.signature.range)),t;if(!Mc(n))return t;let i=Tc(n.params,n.optParams,n.restParam,t.range),a=n.optParams.length>0,o=a?jc(n.params.length,n.optParams,n.restParam,i,t.range):i,s=a?N(Ec,t.range):n.restParam?N(n.restParam.name,t.range):void 0,c=ve([{pat:N(vc,t.range),value:t.value}],U(N(yc,t.range),[N(vc,t.range),ie(n.params.map(e=>N(e.name,t.range)),o,t.range,s)],t.range),t.range);return t.tag===`define`?T(t.name,c,t.range,t.docComments):We(t.name,c,t.range,t.docComments)}function Pc(e,t){return t.map(t=>Nc(e,t))}function Fc(e,t,n={}){let r=[],i=Bs(r,e,n);if(r.length>0)return{diagnostics:r};if(!t)return{program:i,diagnostics:r};let a=dc(i,t);if(!a.ok)return{diagnostics:[a.diagnostic]};let o=a.prog,s=a.range.firstLineSpan(e),c=o.find(e=>e.range.contains(t));if(c?.tag!==`define`&&c?.tag!==`defexport`||c.docComments===void 0)return{diagnostics:[J(`Query`,`error`,`Querying is only allowed within function definitions with docstrings`)]};let{doc:l,diagnostics:u}=Za(c.docComments);if(u.length>0)return{diagnostics:u};if(l===void 0)return{diagnostics:[J(`Query`,`error`,`Querying is only allowed within function definitions with docstrings`)]};let d=l.tags.filter(e=>ka(e)).at(0);return d?(o.push(Un(d.contents.functionCall)),{program:o,queriedRange:s,diagnostics:[]}):{diagnostics:[J(`Query`,`error`,`Querying requires an example tag`)]}}async function Ic(e,t={}){let{queryLoc:n,insertContracts:r=!1,scopeCheck:i=!1,allowInternalNames:a=!1}=t,o=n?Fc(e,n,{allowInternalNames:a}):Fc(e,void 0,{allowInternalNames:a}),s=[...o.diagnostics];if(o.program===void 0)return n?{queriedRange:void 0,diagnostics:s}:{diagnostics:s};let c=o.program;i&&await ac(s,$i(c));let l=Xi($i(r?Pc(s,c):c));return n?{prog:l,queriedRange:o.queriedRange,diagnostics:s}:{prog:l,diagnostics:s}}function Lc(e){let{program:t,diagnostics:n}=Fc(e);return t===void 0?{examples:[],diagnostics:n}:{examples:hc(t).map(e=>({range:e.range,prog:Xi($i(gc(t,e)))})),diagnostics:n}}function Rc(e){return Fc(e).program?.length}function zc(){return Yn.empty.extendWithImport(`runtime`,Li.get(`runtime`)).extendWithImport(`prelude`,Li.get(`prelude`))}var Bc=60,Vc=class{tasks=[];steppingGates=new Map;tracesStarted=new Set;nextCaption=new Map;steppingTaskId;currTaskIdx=0;timeQuantum=1e3/Bc;currentLoop=null;currentTaskId(){return this.steppingTaskId}schedule(e){if(e.fiber.isDone())throw new H(`Scheduler.schedule`,`Scheduling invariant violated: scheduling completed fibers is disallowed!`);Hc(e)&&e.stepping&&!this.steppingGates.has(e.id)&&this.steppingGates.set(e.id,{task:e,mode:`step`,resolve:()=>{},lastStmtIdx:e.fiber.stmtIndex,parked:!1}),this.tasks.push(e),this.resumeExecution()}cancelTask(e){let t=this.wasPaused();this.pauseExecution();let n=this.steppingGates.get(e),r=this.tasks.findIndex(t=>t.id===e);if(n===void 0&&r===-1){t||this.resumeExecution();return}(n?.task.err??this.tasks[r].err).report(new L(`Runtime`,`Evaluation cancelled`)),r!==-1&&this.tasks.splice(r,1),n&&(this.steppingGates.delete(e),n.resolve()),this.tracesStarted.delete(e),this.nextCaption.delete(e),t||this.resumeExecution()}pauseExecution(){this.currentLoop=null}resumeExecution(){if(this.currentLoop!==null)return;let e=Symbol(`execute loop`);this.currentLoop=e,this.execute(e)}stepTask(e){let{fiber:t}=e;if(t.isDone())throw new H(`Scheduler.execute`,`Scheduling invariant violated: a completed fiber remains in the task queue!`);try{return t.step()}catch(n){if(n instanceof cr)throw new H(`Scheduler.stepTask`,`A set-recursion-depth signal escaped applyFn!`);if(n instanceof Dn)return Vi(n.action,n.range);if(!(n instanceof L))throw n;if(!(n instanceof fn)&&t.handleError(n))return;if(Uc(e)){console.debug(n),e.err.report(n),this.endCurrFiber(e);return}this.reportAndUnwind(n,e);return}}async processStepResult(e,t){let n=t.fiber;if(!e)return!1;if(e.tag===`import-file`){let{getFS:r}=await ii(async()=>{let{getFS:e}=await import(`./fs-43.4.0-rc.1.js`);return{getFS:e}},__vite__mapDeps([0,1,2,3])),i;try{i=await r().fileExists(e.filename)}catch(n){return t.err.report(n instanceof L?n:new L(`Runtime`,`Attempted to import file "${e.filename}" but it could not be read!`)),this.endCurrFiber(t),!0}return i?(this.removeTaskFromQueue(t),r().loadFile(e.filename).then(async r=>{let{prog:i,diagnostics:a}=await Ic(r);if(a.forEach(e=>{t.err.report(ea(e))}),i===void 0){n.advanceStmt(),this.resumeOrComplete(t);return}let o=new Hi(i,zc(),`import`),s=crypto.randomUUID(),c=()=>{let r=o.getModule();n.topLevelEnv=e.alias===void 0?n.topLevelEnv.extendWithImport(e.filename,r):n.topLevelEnv.extendWithQualifiedImport(e.alias,r),n.advanceStmt(),this.resumeOrComplete(t)};if(o.isDone()){c();return}this.schedule({id:s,fiber:o,err:t.err,onFatal:r=>{t.err.report(new L(`Runtime`,`Attempted to import file "${e.filename}" but it failed to run: ${r instanceof Error?r.toString():String(r)}`)),n.advanceStmt(),this.resumeOrComplete(t)},onComplete:c})},r=>{t.err.report(new L(`Runtime`,`Attempted to import file "${e.filename}" but it failed to load!`)),n.advanceStmt(),this.resumeOrComplete(t)})):(t.err.report(new L(`Runtime`,`Attempted to import file "${e.filename}" but it does not exist!`)),this.endCurrFiber(t)),!0}if(e.tag===`block-on`)return this.removeTaskFromQueue(t),e.action().then(e=>{n.resumeWithValue(e),this.schedule(t)},r=>{let i=r instanceof L?r:new L(`Runtime`,r instanceof Error?r.message:String(r));i.range??=e.range,n.handleError(i)||(t.err.report(i),n.advanceStmt()),this.resumeOrComplete(t)}),!0;if(!Hc(t))return!1;let{out:r}=t,i=t.stepping?this.steppingGates.get(t.id):void 0,a=e.tag===`minor`||e.tag===`yield`,o=!1;if(e.tag===`display`){if(t.isTracing&&t.stepper&&n.lastResult!==null){let e=t.stepper.final(n.lastResult);e!==void 0&&(this.captionUpTo(t,n.stmtIndex-1),r.send(this.mkTraceValue(t,e)),o=!0)}else this.captionUpTo(t,n.stmtIndex-1),r.send(n.lastResult)}else if(!a&&t.isTracing&&t.stepper){let e=t.stepper.render(n);e!==void 0&&(this.captionUpTo(t,n.stmtIndex),r.send(this.mkTraceValue(t,e)),o=!0)}if(i){let e=n.stmtIndex>i.lastStmtIdx;if(i.lastStmtIdx=n.stmtIndex,!n.isDone()&&(i.mode===`step`?o:i.mode===`statement`&&e))return this.parkInGate(t),!0}return!1}mkTraceValue(e,t){return this.tracesStarted.has(e.id)?Ki(t):(this.tracesStarted.add(e.id),Gi(``,t))}captionUpTo(e,t){let n=e.src,r=e.out.beginStatement;if(n===void 0||r===void 0)return;let i=this.nextCaption.get(e.id)??0,a=i>0?e.fiber.statementAt(i-1)?.range:void 0;for(;i<=t;i++){let t=e.fiber.statementAt(i);if(t===void 0)break;let{begin:o,end:s}=t.range;if(o.idx<0||s.idx<o.idx||s.idx>=n.length||a!==void 0&&tn(a,t.range))continue;let c=n.slice(o.idx,s.idx+1).trim();c.length>0&&(r.call(e.out,c,i),a=t.range)}this.nextCaption.set(e.id,i)}captionRemaining(e){Hc(e)&&e.fiber.isDone()&&this.captionUpTo(e,e.fiber.statementCount-1)}parkInGate(e){this.removeTaskFromQueue(e);let t=this.steppingGates.get(e.id);if(t){t.parked=!0;let e=t.resolve;t.resolve=()=>{},e()}}wakeGate(e){let t=this.steppingGates.get(e);if(t?.parked){if(t.task.fiber.isDone()){this.steppingGates.delete(e),this.tracesStarted.delete(e),this.nextCaption.delete(e);let n=t.resolve;t.task.onComplete?.(),n();return}t.parked=!1,this.schedule(t.task)}}step(e){let t=this.steppingGates.get(e);t&&(t.mode=`step`,this.wakeGate(e))}resume(e,t){let n=this.steppingGates.get(e);if(!n)return Promise.resolve();let r=n.resolve;return n.resolve=()=>{},r(),new Promise(r=>{n.mode=t,n.resolve=r,this.wakeGate(e)})}pauseStepping(e){let t=this.steppingGates.get(e);t&&(t.mode=`step`)}async execute(e){for(;this.currentLoop===e;){if(this.tasks.length===0){this.retire(e);return}await Wi();let t=performance.now();for(;performance.now()-t<this.timeQuantum&&this.currentLoop===e;){if(this.currTaskIdx>=this.tasks.length){if(this.tasks.length===0)break;this.currTaskIdx=0}let t=this.tasks.at(this.currTaskIdx);if(!t)throw new H(`Scheduler.execute`,`Scheduler attempted to execute task #${this.currTaskIdx.toString()} when there are only ${this.tasks.length.toString()} tasks!`);try{this.steppingTaskId=t.id;let e;try{e=this.stepTask(t)}finally{this.steppingTaskId=void 0}await this.processStepResult(e,t)||this.moveNextTask(t)}catch(n){if(this.dropTask(t),t.onFatal===void 0)throw this.retire(e),n;t.onFatal(n)}}}}dropTask(e){let t=this.tasks.findIndex(t=>t.id===e.id);t!==-1&&this.tasks.splice(t,1);let n=this.steppingGates.get(e.id);n&&(this.steppingGates.delete(e.id),n.resolve()),this.tracesStarted.delete(e.id),this.nextCaption.delete(e.id)}removeTaskFromQueue(e){let t=this.tasks.findIndex(t=>t.id===e.id);return t!==-1&&(this.tasks[t]=this.tasks[this.tasks.length-1],this.tasks.pop(),!0)}endCurrFiber(e){if(!this.removeTaskFromQueue(e))return;this.captionRemaining(e),this.tracesStarted.delete(e.id),this.nextCaption.delete(e.id);let t=this.steppingGates.get(e.id);t&&(this.steppingGates.delete(e.id),t.resolve()),e.onComplete?.()}resumeOrComplete(e){e.fiber.isDone()?(this.captionRemaining(e),this.tracesStarted.delete(e.id),this.nextCaption.delete(e.id),e.onComplete?.()):this.schedule(e)}moveNextTask(e){if(!e.fiber.isDone()){let t=this.tasks.findIndex(t=>t.id===e.id);t!==-1&&(this.currTaskIdx=t+1);return}this.endCurrFiber(e)}reportAndUnwind(e,t){this.captionUpTo(t,t.fiber.stmtIndex),t.err.report(e),t.fiber.advanceStmt()}retire(e){this.currentLoop===e&&(this.currentLoop=null)}wasPaused(){return this.currentLoop===null}async setTimeQuantumFromFPS(){let e=await new Promise(e=>{let t=0,n=performance.now();function r(){t++;let i=performance.now()-n;if(i>=1e3){e(1e3/Math.floor(t*1e3/i));return}requestAnimationFrame(r)}requestAnimationFrame(r)});this.timeQuantum=e}};function Hc(e){return typeof e==`object`&&`out`in e&&`isTracing`in e}function Uc(e){return!Hc(e)}var Wc=0;function Gc(e,t){if(e.isDone())return Promise.resolve();let n=new Vc;return new Promise((r,i)=>{n.schedule({id:`run-${(Wc++).toString()}`,fiber:e,out:t.out,err:t.err,isTracing:t.isTracing??!1,stepper:t.stepper,src:t.src,onComplete:r,onFatal:i})}).finally(()=>{n.pauseExecution()})}var Kc=t({canvas_animateWith:()=>rl,canvas_canvasCircle:()=>$c,canvas_canvasDrawing:()=>tl,canvas_canvasEllipse:()=>Qc,canvas_canvasGetPixel:()=>al,canvas_canvasHeight:()=>Yc,canvas_canvasOnclick:()=>il,canvas_canvasPath:()=>nl,canvas_canvasQ:()=>qc,canvas_canvasRectangle:()=>Zc,canvas_canvasSetPixels:()=>ll,canvas_canvasText:()=>el,canvas_canvasToPixels:()=>sl,canvas_canvasWidth:()=>Jc,canvas_makeCanvas:()=>Xc,canvas_pixelsQ:()=>ol,canvas_pixelsToCanvas:()=>cl});function qc(e){return typeof HTMLCanvasElement<`u`&&e instanceof HTMLCanvasElement}function Jc(e){return e.width}function Yc(e){return e.height}function Xc(e,t){B();let n=document.createElement(`canvas`);return n.width=e,n.height=t,n}function Zc(e,t,n,r,i,a,o){let s=pn(e);if(s.fillStyle=Pn(Xr(o)),s.strokeStyle=Pn(Xr(o)),a===`solid`)s.fillRect(t,n,r,i);else if(a===`outline`)s.strokeRect(t,n,r,i);else throw new L(`Runtime`,`canvas-rectangle!: expected "solid" or "outline", but got ${a}`)}function Qc(e,t,n,r,i,a,o,s,c,l){let u=pn(e);if(u.fillStyle=Pn(Xr(l)),u.strokeStyle=Pn(Xr(l)),u.beginPath(),u.ellipse(t,n,r,i,a,o,s),c===`solid`)u.fill();else if(c===`outline`)u.stroke();else throw new L(`Runtime`,`canvas-ellipse!: expected "solid" or "outline", but got ${c}`)}function $c(e,t,n,r,i,a){let o=pn(e);if(o.fillStyle=Pn(Xr(a)),o.strokeStyle=Pn(Xr(a)),o.beginPath(),o.arc(t,n,r,0,2*Math.PI),i===`solid`)o.fill();else if(i===`outline`)o.stroke();else throw new L(`Runtime`,`canvas-circle!: expected "solid" or "outline", but got ${i}`)}function el(e,t,n,r,i,a,o,s){let c=s??bn(`Arial`),l=pn(e);if(l.fillStyle=Pn(Xr(o)),l.strokeStyle=Pn(Xr(o)),l.font=Xn(c,i),a===`solid`)l.fillText(r,t,n);else if(a===`outline`)l.strokeText(r,t,n);else throw new L(`Runtime`,`canvas-text!: expected "solid" or "outline", but got ${a}`)}function tl(e,t,n,r){ye(t,n,mn(r),e)}function nl(e,t,n,r){let i=pn(e),a=Ln(t);if(n!==`solid`&&n!==`outline`)throw new L(`Runtime`,`canvas-path!: expected "solid" or "outline", but got ${n}`);if(a.length===0)return;i.fillStyle=Pn(Xr(r)),i.strokeStyle=Pn(Xr(r)),i.beginPath();let o=a[0];i.moveTo(o.fst,o.snd);for(let e=1;e<a.length;e++)o=a[e],i.lineTo(o.fst,o.snd);n===`solid`?i.fill():i.stroke()}function rl(e){B();let t=z();function n(r){t.signal?.aborted||t.spawn(e,[r],e=>{e===!0&&!t.signal?.aborted&&window.requestAnimationFrame(n)})}window.requestAnimationFrame(n)}function il(e,t){let n=z();e.addEventListener(`click`,e=>{n.spawn(t,[e.offsetX,e.offsetY])},{signal:n.signal})}function al(e,t,n){let r=pn(e).getImageData(t,n,1,1).data;return Je(r[0],r[1],r[2],r[3])}function ol(e){return nn(e)&&e.every(e=>c(e,`rgba`))}function sl(e){let t=pn(e).getImageData(0,0,e.width,e.height).data,n=[];for(let e=0;e<t.length;e+=4)n.push(Je(t[e],t[e+1],t[e+2],t[e+3]));return n}function cl(e,t,n){B();let r=document.createElement(`canvas`);r.width=t,r.height=n;let i=pn(r),a=i.createImageData(t,n),o=a.data;for(let t=0;t<e.length;t++){let n=e[t];o[t*4]=n.red,o[t*4+1]=n.green,o[t*4+2]=n.blue,o[t*4+3]=n.alpha}return i.putImageData(a,0,0),r}function ll(e,t){let n=pn(e),r=n.createImageData(e.width,e.height),i=r.data;for(let e=0;e<t.length;e++){let n=t[e];i[e*4]=n.red,i[e*4+1]=n.green,i[e*4+2]=n.blue,i[e*4+3]=n.alpha}n.putImageData(r,0,0)}var ul=e(n(((e,t)=>{((n,r)=>{typeof define==`function`&&define.amd?define([],r):typeof t==`object`&&e!==void 0?t.exports=r():n.Papa=r()})(e,function e(){var t=typeof self<`u`?self:typeof window<`u`?window:t===void 0?{}:t,n=!t.document&&!!t.postMessage,r=t.IS_PAPA_WORKER||!1,i={},a=0,o={};function s(e){return e.charCodeAt(0)===65279?e.slice(1):e}function c(e){this._handle=null,this._finished=!1,this._completed=!1,this._halted=!1,this._input=null,this._baseIndex=0,this._partialLine=``,this._rowCount=0,this._start=0,this._nextChunk=null,this.isFirstChunk=!0,this._completeResults={data:[],errors:[],meta:{}},function(e){var t=y(e);t.chunkSize=parseInt(t.chunkSize),e.step||e.chunk||(t.chunkSize=null),this._handle=new p(t),(this._handle.streamer=this)._config=t}.call(this,e),this.parseChunk=function(e,n){var i=parseInt(this._config.skipFirstNLines)||0;if(this.isFirstChunk&&0<i){let t=this._config.newline;t||=(a=this._config.quoteChar||`"`,this._handle.guessLineEndings(e,a)),e=[...e.split(t).slice(i)].join(t)}this.isFirstChunk&&x(this._config.beforeFirstChunk)&&(a=this._config.beforeFirstChunk(e))!==void 0&&(e=a),this.isFirstChunk=!1,this._halted=!1;var i=this._partialLine+e,a=(this._partialLine=``,this._handle.parse(i,this._baseIndex,!this._finished));if(!this._handle.paused()&&!this._handle.aborted()){if(e=a.meta.cursor,i=(this._finished||(this._partialLine=i.substring(e-this._baseIndex),this._baseIndex=e),a&&a.data&&(this._rowCount+=a.data.length),this._finished||this._config.preview&&this._rowCount>=this._config.preview),r)t.postMessage({results:a,workerId:o.WORKER_ID,finished:i});else if(x(this._config.chunk)&&!n){if(this._config.chunk(a,this._handle),this._handle.paused()||this._handle.aborted())return void(this._halted=!0);this._completeResults=a=void 0}return this._config.step||this._config.chunk||(this._completeResults.data=this._completeResults.data.concat(a.data),this._completeResults.errors=this._completeResults.errors.concat(a.errors),this._completeResults.meta=a.meta),this._completed||!i||!x(this._config.complete)||a&&a.meta.aborted||(this._config.complete(this._completeResults,this._input),this._completed=!0),i||a&&a.meta.paused||this._nextChunk(),a}this._halted=!0},this._sendError=function(e){x(this._config.error)?this._config.error(e):r&&this._config.error&&t.postMessage({workerId:o.WORKER_ID,error:e,finished:!1})}}function l(e){var t;(e||={}).chunkSize||(e.chunkSize=o.RemoteChunkSize),c.call(this,e),this._nextChunk=n?function(){this._readChunk(),this._chunkLoaded()}:function(){this._readChunk()},this.stream=function(e){this._input=e,this._nextChunk()},this._readChunk=function(){if(this._finished)this._chunkLoaded();else{if(t=new XMLHttpRequest,this._config.withCredentials&&(t.withCredentials=this._config.withCredentials),n||(t.onload=b(this._chunkLoaded,this),t.onerror=b(this._chunkError,this)),t.ontimeout=b(this._chunkTimeout,this),t.open(this._config.downloadRequestBody?`POST`:`GET`,this._input,!n),this._config.downloadTimeout&&!n&&(t.timeout=this._config.downloadTimeout),this._config.downloadRequestHeaders){var e,r=this._config.downloadRequestHeaders;for(e in r)t.setRequestHeader(e,r[e])}var i;this._config.chunkSize&&(i=this._start+this._config.chunkSize-1,t.setRequestHeader(`Range`,`bytes=`+this._start+`-`+i));try{t.send(this._config.downloadRequestBody)}catch(e){this._chunkError(e.message)}n&&t.status===0&&this._chunkError()}},this._chunkLoaded=function(){t.readyState===4&&(t.status<200||400<=t.status?this._chunkError():(this._start+=this._config.chunkSize||t.responseText.length,this._finished=!this._config.chunkSize||this._start>=(e=>(e=e.getResponseHeader(`Content-Range`))===null?-1:parseInt(e.substring(e.lastIndexOf(`/`)+1)))(t),this.parseChunk(t.responseText)))},this._chunkError=function(e){e=t.statusText||e,this._sendError(Error(e))},this._chunkTimeout=function(){this._chunkError(`Request timed out after `+this._config.downloadTimeout+`ms`)}}function u(e){(e||={}).chunkSize||(e.chunkSize=o.LocalChunkSize),c.call(this,e);var t,n,r=typeof FileReader<`u`;this.stream=function(e){this._input=e,n=e.slice||e.webkitSlice||e.mozSlice,r?((t=new FileReader).onload=b(this._chunkLoaded,this),t.onerror=b(this._chunkError,this)):t=new FileReaderSync,this._nextChunk()},this._nextChunk=function(){this._finished||this._config.preview&&!(this._rowCount<this._config.preview)||this._readChunk()},this._readChunk=function(){var e=this._input,i=(this._config.chunkSize&&(i=Math.min(this._start+this._config.chunkSize,this._input.size),e=n.call(e,this._start,i)),t.readAsText(e,this._config.encoding));r||this._chunkLoaded({target:{result:i}})},this._chunkLoaded=function(e){this._start+=this._config.chunkSize,this._finished=!this._config.chunkSize||this._start>=this._input.size,this.parseChunk(e.target.result)},this._chunkError=function(){this._sendError(t.error)}}function d(e){var t;c.call(this,e||={}),this.stream=function(e){return t=e,this._nextChunk()},this._nextChunk=function(){var e,n;if(!this._finished)return e=this._config.chunkSize,t=e?(n=t.substring(0,e),t.substring(e)):(n=t,``),this._finished=!t,this.parseChunk(n)}}function f(e){c.call(this,e||={});var t=[],n=!0,r=!1;this.pause=function(){c.prototype.pause.apply(this,arguments),this._input.pause()},this.resume=function(){c.prototype.resume.apply(this,arguments),this._input.resume()},this.stream=function(e){this._input=e,this._input.on(`data`,this._streamData),this._input.on(`end`,this._streamEnd),this._input.on(`error`,this._streamError)},this._checkIsFinished=function(){r&&t.length===1&&(this._finished=!0)},this._nextChunk=function(){this._checkIsFinished(),t.length?this.parseChunk(t.shift()):n=!0},this._streamData=b(function(e){try{t.push(typeof e==`string`?e:e.toString(this._config.encoding)),n&&(n=!1,this._checkIsFinished(),this.parseChunk(t.shift()))}catch(e){this._streamError(e)}},this),this._streamError=b(function(e){this._streamCleanUp(),this._sendError(e)},this),this._streamEnd=b(function(){this._streamCleanUp(),r=!0,this._streamData(``)},this),this._streamCleanUp=b(function(){this._input.removeListener(`data`,this._streamData),this._input.removeListener(`end`,this._streamEnd),this._input.removeListener(`error`,this._streamError)},this)}function p(e){var t,n,r,i,a=2**53,s=-a,c=/^\s*-?(\d+\.?|\.\d+|\d+\.\d+)([eE][-+]?\d+)?\s*$/,l=/^((\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d\.\d+([+-][0-2]\d:[0-5]\d|Z))|(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d:[0-5]\d([+-][0-2]\d:[0-5]\d|Z))|(\d{4}-[01]\d-[0-3]\dT[0-2]\d:[0-5]\d([+-][0-2]\d:[0-5]\d|Z)))$/,u=this,d=0,f=0,p=!1,g=!1,_=[],v={data:[],errors:[],meta:{}};function b(t){return e.skipEmptyLines===`greedy`?t.join(``).trim()===``:t.length===1&&t[0].length===0}function S(){if(v&&r&&(w(`Delimiter`,`UndetectableDelimiter`,`Unable to auto-detect delimiting character; defaulted to '`+o.DefaultDelimiter+`'`),r=!1),e.skipEmptyLines&&(v.data=v.data.filter(function(e){return!b(e)})),C()){if(v){if(Array.isArray(v.data[0])){for(var t=0;C()&&t<v.data.length;t++)v.data[t].forEach(e);v.data.splice(0,1)}else v.data.forEach(e)}function e(e){_.push(e)}}function n(t,n){for(var r=e.header?{}:[],i=0;i<t.length;i++){var o=i,u=t[i],u=((t,n)=>(t=>(e.dynamicTypingFunction&&e.dynamicTyping[t]===void 0&&(e.dynamicTyping[t]=e.dynamicTypingFunction(t)),!0===(e.dynamicTyping[t]||e.dynamicTyping)))(t)?n===`true`||n===`TRUE`||n!==`false`&&n!==`FALSE`&&((e=>{if(c.test(e)&&(e=parseFloat(e),s<e&&e<a))return 1})(n)?parseFloat(n):l.test(n)?new Date(n):n===``?null:n):n)(o=e.header?i>=_.length?`__parsed_extra`:_[i]:o,u=e.transform?e.transform(u,o):u);o===`__parsed_extra`?(r[o]=r[o]||[],r[o].push(u)):r[o]=u}return e.header&&(i>_.length?w(`FieldMismatch`,`TooManyFields`,`Too many fields: expected `+_.length+` fields but parsed `+i,f+n):i<_.length&&w(`FieldMismatch`,`TooFewFields`,`Too few fields: expected `+_.length+` fields but parsed `+i,f+n)),r}var i;v&&(e.header||e.dynamicTyping||e.transform)&&(i=1,!v.data.length||Array.isArray(v.data[0])?(v.data=v.data.map(n),i=v.data.length):v.data=n(v.data,0),e.header&&v.meta&&(v.meta.fields=_),f+=i)}function C(){return e.header&&_.length===0}function w(e,t,n,r){e={type:e,code:t,message:n},r!==void 0&&(e.row=r),v.errors.push(e)}x(e.step)&&(i=e.step,e.step=function(t){v=t,C()?S():(S(),v.data.length!==0&&(d+=t.data.length,e.preview&&d>e.preview?n.abort():(v.data=v.data[0],i(v,u))))}),this.parse=function(i,a,s){var c=e.quoteChar||`"`,c=(e.newline||=this.guessLineEndings(i,c),r=!1,e.delimiter?x(e.delimiter)&&(e.delimiter=e.delimiter(i),v.meta.delimiter=e.delimiter):((c=((t,n,r,i,a)=>{var s,c,l,u;a||=[`,`,`	`,`|`,`;`,o.RECORD_SEP,o.UNIT_SEP];for(var d=0;d<a.length;d++){for(var f,p=a[d],m=0,g=0,_=0,v=(l=void 0,new h({comments:i,delimiter:p,newline:n,preview:10}).parse(t)),y=0;y<v.data.length;y++)r&&b(v.data[y])?_++:(f=v.data[y].length,g+=f,l===void 0?l=f:0<f&&(m+=Math.abs(f-l),l=f));0<v.data.length&&(g/=v.data.length-_),1.99<g&&(c===void 0||m<c||m===c&&u<g)&&(c=m,s=p,u=g)}return{successful:!!(e.delimiter=s),bestDelimiter:s}})(i,e.newline,e.skipEmptyLines,e.comments,e.delimitersToGuess)).successful?e.delimiter=c.bestDelimiter:(r=!0,e.delimiter=o.DefaultDelimiter),v.meta.delimiter=e.delimiter),y(e));return c.header=C(),e.preview&&e.header&&c.preview++,t=i,n=new h(c),v=n.parse(t,a,s),S(),p?{meta:{paused:!0}}:v||{meta:{paused:!1}}},this.paused=function(){return p},this.pause=function(){p=!0,n.abort(),t=x(e.chunk)?``:t.substring(n.getCharIndex())},this.resume=function(){u.streamer._halted?(p=!1,u.streamer.parseChunk(t,!0)):setTimeout(u.resume,3)},this.aborted=function(){return g},this.abort=function(){g=!0,n.abort(),v.meta.aborted=!0,x(e.complete)&&e.complete(v),t=``},this.guessLineEndings=function(e,t){e=e.substring(0,1048576);var t=RegExp(m(t)+`([^]*?)`+m(t),`gm`),n=(e=e.replace(t,``)).split(`\r`),t=e.split(`
`),e=1<t.length&&t[0].length<n[0].length;if(n.length===1||e)return`
`;for(var r=0,i=0;i<n.length;i++)n[i][0]===`
`&&r++;return r>=n.length/2?`\r
`:`\r`}}function m(e){return e.replace(/[.*+?^${}()|[\]\\]/g,`\\$&`)}function h(e){var t=(e||={}).delimiter,n=e.newline,r=e.comments,i=e.step,a=e.preview,c=e.fastMode,l=null,u=!1,d=e.quoteChar==null?`"`:e.quoteChar,f=d;if(e.escapeChar!==void 0&&(f=e.escapeChar),(typeof t!=`string`||-1<o.BAD_DELIMITERS.indexOf(t))&&(t=`,`),r===t)throw Error(`Comment character same as delimiter`);!0===r?r=`#`:(typeof r!=`string`||-1<o.BAD_DELIMITERS.indexOf(r))&&(r=!1),n!==`
`&&n!==`\r`&&n!==`\r
`&&(n=`
`);var p=0,h=!1;this.parse=function(o,g,_){if(typeof o!=`string`)throw Error(`Input must be a string`);var v=o.length,y=t.length,b=n.length,S=r.length,C=x(i),w=[],T=[],E=[],ee=p=0;if(!o)return j();if(c||!1!==c&&o.indexOf(d)===-1){for(var te=o.split(n),D=0;D<te.length;D++){if(E=te[D],p+=E.length,D!==te.length-1)p+=n.length;else if(_)return j();if(!r||E.substring(0,S)!==r){if(C){if(w=[],ie(E.split(t)),ce(),h)return j()}else ie(E.split(t));if(a&&a<=D)return w=w.slice(0,a),j(!0)}}return j()}for(var O=o.indexOf(t,p),k=o.indexOf(n,p),ne=new RegExp(m(f)+m(d),`g`),A=o.indexOf(d,p);;)if(o[p]===d)for(A=p,p++;;){if((A=o.indexOf(d,A+1))===-1)return _||T.push({type:`Quotes`,code:`MissingQuotes`,message:`Quoted field unterminated`,row:w.length,index:p}),oe();if(A===v-1)return oe(o.substring(p,A).replace(ne,d));if(d===f&&o[A+1]===f)A++;else if(d===f||A===0||o[A-1]!==f){O!==-1&&O<A+1&&(O=o.indexOf(t,A+1));var re=ae((k=k!==-1&&k<A+1?o.indexOf(n,A+1):k)===-1?O:Math.min(O,k));if(o.substr(A+1+re,y)===t){E.push(o.substring(p,A).replace(ne,d)),o[p=A+1+re+y]!==d&&(A=o.indexOf(d,p)),O=o.indexOf(t,p),k=o.indexOf(n,p);break}if(re=ae(k),o.substring(A+1+re,A+1+re+b)===n){if(E.push(o.substring(p,A).replace(ne,d)),se(A+1+re+b),O=o.indexOf(t,p),A=o.indexOf(d,p),C&&(ce(),h))return j();if(a&&w.length>=a)return j(!0);break}T.push({type:`Quotes`,code:`InvalidQuotes`,message:`Trailing quote on quoted field is malformed`,row:w.length,index:p}),A++}}else if(r&&E.length===0&&o.substring(p,p+S)===r){if(k===-1)return j();p=k+b,k=o.indexOf(n,p),O=o.indexOf(t,p)}else if(O!==-1&&(O<k||k===-1))E.push(o.substring(p,O)),p=O+y,O=o.indexOf(t,p);else{if(k===-1)break;if(E.push(o.substring(p,k)),se(k+b),C&&(ce(),h))return j();if(a&&w.length>=a)return j(!0)}return oe();function ie(e){w.push(e),ee=p}function ae(e){var t=0;return t=e!==-1&&(e=o.substring(A+1,e))&&e.trim()===``?e.length:t}function oe(e){return _||(e===void 0&&(e=o.substring(p)),E.push(e),p=v,ie(E),C&&ce()),j()}function se(e){p=e,ie(E),E=[],k=o.indexOf(n,p)}function j(r){if(e.header&&!g&&w.length&&!u){var i=w[0],a=Object.create(null),o=new Set(i);let t=!1;for(let n=0;n<i.length;n++){let r=s(i[n]);if(a[r=x(e.transformHeader)?e.transformHeader(r,n):r]){let e,s=a[r];for(;e=r+`_`+s,s++,o.has(e););o.add(e),i[n]=e,a[r]++,t=!0,(l=l===null?{}:l)[e]=r}else a[r]=1,i[n]=r;o.add(r)}t&&console.warn(`Duplicate headers found and renamed.`),u=!0}return{data:w,errors:T,meta:{delimiter:t,linebreak:n,aborted:h,truncated:!!r,cursor:ee+(g||0),renamedHeaders:l}}}function ce(){i(j()),w=[],T=[]}},this.abort=function(){h=!0},this.getCharIndex=function(){return p}}function g(e){var t=e.data,n=i[t.workerId],r=!1;if(t.error)n.userError(t.error,t.file);else if(t.results&&t.results.data){var a={abort:function(){r=!0,_(t.workerId,{data:[],errors:[],meta:{aborted:!0}})},pause:v,resume:v};if(x(n.userStep)){for(var o=0;o<t.results.data.length&&(n.userStep({data:t.results.data[o],errors:t.results.errors,meta:t.results.meta},a),!r);o++);delete t.results}else x(n.userChunk)&&(n.userChunk(t.results,a,t.file),delete t.results)}t.finished&&!r&&_(t.workerId,t.results)}function _(e,t){var n=i[e];x(n.userComplete)&&n.userComplete(t),n.terminate(),delete i[e]}function v(){throw Error(`Not implemented.`)}function y(e){if(typeof e!=`object`||!e)return e;var t,n=Array.isArray(e)?[]:{};for(t in e)n[t]=y(e[t]);return n}function b(e,t){return function(){e.apply(t,arguments)}}function x(e){return typeof e==`function`}return o.parse=function(n,r){var c=(r||={}).dynamicTyping||!1;if(x(c)&&(r.dynamicTypingFunction=c,c={}),r.dynamicTyping=c,r.transform=!!x(r.transform)&&r.transform,r.downloadTimeout!==void 0){var c=parseInt(r.downloadTimeout);if(isNaN(c))throw Error(`Config downloadTimeout value (`+r.downloadTimeout+`) not parsable by parseInt(val).`);r.downloadTimeout=c}if(!r.worker||!o.WORKERS_SUPPORTED)return c=null,o.NODE_STREAM_INPUT,typeof n==`string`?(n=s(n),c=new(r.download?l:d)(r)):!0===n.readable&&x(n.read)&&x(n.on)?c=new f(r):(t.File&&n instanceof File||n instanceof Object)&&(c=new u(r)),c.stream(n);(c=(()=>{var n;return!!o.WORKERS_SUPPORTED&&(n=(()=>{var n=t.URL||t.webkitURL||null,r=e.toString();return o.BLOB_URL||=n.createObjectURL(new Blob([`var global = (function() { if (typeof self !== 'undefined') { return self; } if (typeof window !== 'undefined') { return window; } if (typeof global !== 'undefined') { return global; } return {}; })(); global.IS_PAPA_WORKER=true; `,`(`,r,`)();`],{type:`text/javascript`}))})(),(n=new t.Worker(n)).onmessage=g,n.id=a++,i[n.id]=n)})()).userStep=r.step,c.userChunk=r.chunk,c.userComplete=r.complete,c.userError=r.error,r.step=x(r.step),r.chunk=x(r.chunk),r.complete=x(r.complete),r.error=x(r.error),delete r.worker,c.postMessage({input:n,config:r,workerId:c.id})},o.unparse=function(e,t){var n=!1,r=!0,i=`,`,a=`\r
`,s=`"`,c=s+s,l=!1,u=null,d=!1,f=((()=>{if(typeof t==`object`){if(typeof t.delimiter!=`string`||o.BAD_DELIMITERS.filter(function(e){return t.delimiter.indexOf(e)!==-1}).length||(i=t.delimiter),typeof t.quotes!=`boolean`&&typeof t.quotes!=`function`&&!Array.isArray(t.quotes)||(n=t.quotes),typeof t.skipEmptyLines!=`boolean`&&typeof t.skipEmptyLines!=`string`||(l=t.skipEmptyLines),typeof t.newline==`string`&&(a=t.newline),typeof t.quoteChar==`string`&&(s=t.quoteChar,c=s+s),typeof t.header==`boolean`&&(r=t.header),Array.isArray(t.columns)){if(t.columns.length===0)throw Error(`Option columns is empty`);u=t.columns}t.escapeChar!==void 0&&(c=t.escapeChar+s),t.escapeFormulae instanceof RegExp?d=t.escapeFormulae:typeof t.escapeFormulae==`boolean`&&t.escapeFormulae&&(d=/^[=+\-@\t\r].*$/)}})(),new RegExp(m(s),`g`));if(typeof e==`string`&&(e=JSON.parse(e)),Array.isArray(e)){if(!e.length||Array.isArray(e[0]))return p(null,e,l);if(typeof e[0]==`object`)return p(u||Object.keys(e[0]),e,l)}else if(typeof e==`object`)return typeof e.data==`string`&&(e.data=JSON.parse(e.data)),Array.isArray(e.data)&&(e.fields||(e.fields=e.meta&&e.meta.fields||u),e.fields||(e.fields=Array.isArray(e.data[0])?e.fields:typeof e.data[0]==`object`?Object.keys(e.data[0]):[]),Array.isArray(e.data[0])||typeof e.data[0]==`object`||(e.data=[e.data])),p(e.fields||[],e.data||[],l);throw Error(`Unable to serialize unrecognized input`);function p(e,t,n){var o=``,s=(typeof e==`string`&&(e=JSON.parse(e)),typeof t==`string`&&(t=JSON.parse(t)),Array.isArray(e)&&0<e.length),c=!Array.isArray(t[0]);if(s&&r){for(var l=0;l<e.length;l++)0<l&&(o+=i),o+=h(e[l],l);0<t.length&&(o+=a)}for(var u=0;u<t.length;u++){var d=(s?e:t[u]).length,f=!1,p=s?Object.keys(t[u]).length===0:t[u].length===0;if(n&&!s&&(f=n===`greedy`?t[u].join(``).trim()===``:t[u].length===1&&t[u][0].length===0),n===`greedy`&&s){for(var m=[],g=0;g<d;g++){var _=c?e[g]:g;m.push(t[u][_])}f=m.join(``).trim()===``}if(!f){for(var v=0;v<d;v++){0<v&&!p&&(o+=i);var y=s&&c?e[v]:v;o+=h(t[u][y],v)}u<t.length-1&&(!n||0<d&&!p)&&(o+=a)}}return o}function h(e,t){var r,a,l;return e==null?``:e.constructor===Date?isNaN(e.getTime())?``:e.toISOString():(l=!1,d&&typeof e==`string`&&d.test(e)&&(e=`'`+e,l=!0),a=(r=e.toString()).replace(f,c),(l=l||!0===n||typeof n==`function`&&n(e,t)||Array.isArray(n)&&n[t]||((e,t)=>{for(var n=0;n<t.length;n++)if(-1<e.indexOf(t[n]))return!0;return!1})(a,o.BAD_DELIMITERS)||-1<a.indexOf(i)||-1<r.indexOf(s)||a.charAt(0)===` `||a.charAt(a.length-1)===` `)?s+a+s:a)}},o.RECORD_SEP=``,o.UNIT_SEP=``,o.BYTE_ORDER_MARK=`﻿`,o.BAD_DELIMITERS=[`\r`,`
`,`"`,o.BYTE_ORDER_MARK],o.WORKERS_SUPPORTED=!n&&!!t.Worker,o.NODE_STREAM_INPUT=1,o.LocalChunkSize=10485760,o.RemoteChunkSize=5242880,o.DefaultDelimiter=`,`,o.Parser=h,o.ParserHandle=p,o.NetworkStreamer=l,o.FileStreamer=u,o.StringStreamer=d,o.ReadableStreamStreamer=f,r&&(t.onmessage=function(e){e=e.data,o.WORKER_ID===void 0&&e&&(o.WORKER_ID=e.workerId),typeof e.input==`string`?t.postMessage({workerId:o.WORKER_ID,results:o.parse(e.input,e.config),finished:!0}):(t.File&&e.input instanceof File||e.input instanceof Object)&&(e=o.parse(e.input,e.config))&&t.postMessage({workerId:o.WORKER_ID,results:e,finished:!0})}),(l.prototype=Object.create(c.prototype)).constructor=l,(u.prototype=Object.create(c.prototype)).constructor=u,(d.prototype=Object.create(d.prototype)).constructor=d,(f.prototype=Object.create(c.prototype)).constructor=f,o})}))(),1);function dl(e){let t=ul.default.parse(e,{header:!1});if(t.errors.length>0){let e=t.errors.map(e=>`${e.type} (row ${e.row}): ${e.message}`).join(`
`);throw new L(`Runtime`,`Error(s) parsing CSV files:\n${e}`)}return V(t.data.map(V))}function fl(e){let t=Array.from(e).map(e=>qe(e));return V(t)}function pl(e){let t=e.split(/\r?\n/g);return V(t)}var ml=class{data;constructor(){this.data=[]}update(e,t,n){for(let n of this.data)if(Gt(n.key,e)){n.value=t(n.value);return}this.data.push({key:e,value:n})}};function hl(e){let t=new ml,n=e;for(;n!==null;){let e=n.head;t.update(e,e=>e+1,1),n=n.tail}return V(t.data.map(e=>Qe(e.key,e.value)))}var gl=t({data_datasetBar:()=>Fr,data_datasetBubble:()=>Jr,data_datasetLine:()=>en,data_datasetPie:()=>Wr,data_datasetPolar:()=>st,data_datasetQ:()=>_,data_datasetRadar:()=>Br,data_datasetScatter:()=>ft,data_parseCsv:()=>dl,data_plotCategory:()=>ee,data_plotLinear:()=>S,data_plotQ:()=>Ve,data_plotRadial:()=>Vn,data_stringToChars:()=>fl,data_stringToLines:()=>pl,data_tallyAll:()=>hl,data_withDatasetOptions:()=>s,data_withPlotOptions:()=>In}),_l=t({file_fileExistsQ:()=>xl,file_fileToLines:()=>Cl,file_fileToString:()=>Sl,file_linesToFile:()=>Tl,file_stringToFile:()=>wl});async function vl(e){let{getFS:t}=await ii(async()=>{let{getFS:e}=await import(`./fs-43.4.0-rc.1.js`);return{getFS:e}},__vite__mapDeps([0,1,2,3])),n=t();if(!await n.fileExists(e))throw new L(`Runtime`,`File "${e}" does not exist`);try{return await n.loadFile(e)}catch(t){throw t instanceof L?t:new L(`Runtime`,`Could not read the file "${e}"`)}}async function yl(e,t){let{getFS:n}=await ii(async()=>{let{getFS:e}=await import(`./fs-43.4.0-rc.1.js`);return{getFS:e}},__vite__mapDeps([0,1,2,3]));try{await n().saveFile(e,t)}catch(t){throw t instanceof L?t:new L(`Runtime`,`Could not write to the file "${e}"`)}}function bl(e){let t=e.split(/\r?\n/g);return t[t.length-1]===``&&t.pop(),t}function xl(e){throw new Dn(async()=>{let{getFS:t}=await ii(async()=>{let{getFS:e}=await import(`./fs-43.4.0-rc.1.js`);return{getFS:e}},__vite__mapDeps([0,1,2,3]));return t().fileExists(e)})}function Sl(e){throw new Dn(async()=>vl(e))}function Cl(e){throw new Dn(async()=>V(bl(await vl(e))))}function wl(e,t){throw new Dn(async()=>{await yl(t,e)})}function Tl(e,t){throw new Dn(async()=>{let n=Ln(e).map(e=>{if(typeof e!=`string`)throw new L(`Runtime`,`lines->file: expected a list of strings, but the list contains ${I(e)}`);return e});await yl(t,n.length===0?``:`${n.join(`
`)}\n`)})}var El=t({html_button:()=>Ml,html_buttonQ:()=>kl,html_isElement:()=>Dl,html_onKeydown:()=>Fl,html_tag:()=>Nl,html_tagSetChildren:()=>Pl,html_textArea:()=>Al,html_textAreaGet:()=>jl,html_textAreaQ:()=>Ol});function Dl(e){return typeof HTMLElement<`u`&&e instanceof HTMLElement}function Ol(e){return typeof HTMLTextAreaElement<`u`&&e instanceof HTMLTextAreaElement}function kl(e){return typeof HTMLButtonElement<`u`&&e instanceof HTMLButtonElement}function Al(e){B();let t=document.createElement(`textarea`);return t.id=e,t}function jl(e){return e.textContent}function Ml(e,t){B();let n=document.createElement(`button`);n.textContent=e;let r=z();return n.addEventListener(`click`,()=>{r.spawn(t,[])},{signal:r.signal}),n}function Nl(e,...t){B();let n=document.createElement(e);if(t.length>0&&pt(t[0])){let e=Ln(t[0]);for(let t of e)if(C(t)){let e=t;if(!He(e.fst))throw new L(`Runtime`,`attribute must be a string: ${dn(e.fst)}`);if(He(e.snd))n.setAttribute(e.fst,e.snd);else throw new L(`Runtime`,`attribute value must be a string: ${dn(e.snd)}`)}t=t.slice(1)}for(let e of t)e instanceof HTMLElement?n.appendChild(e):n.textContent=e;return n}function Pl(e,...t){if(B(),e instanceof HTMLElement){t.forEach((t,n)=>{if(!(t instanceof HTMLElement))throw new L(`Runtime`,`tag-set-children! expects all children to be HTML elements, but position ${n} is a ${I(e)}$.`)}),e.replaceChildren(t),e.textContent=``;for(let n of t)e.appendChild(n)}else throw new L(`Runtime`,`tag-set-children! expects an HTML element, but received ${I(e)}`)}function Fl(e){B();let t=z();window.addEventListener(`keydown`,n=>{t.spawn(e,[n.key])},{signal:t.signal})}var Il=t({color_allColorNames:()=>On,color_colorNameToRgb:()=>Cn,color_colorQ:()=>Kt,color_colorToColorName:()=>Lr,color_colorToRgb:()=>Xr,color_describeColor:()=>rn,color_findColors:()=>Kr,color_hsv:()=>lt,color_hsvAlpha:()=>y,color_hsvComplement:()=>Hr,color_hsvHue:()=>mt,color_hsvSaturation:()=>D,color_hsvToRgb:()=>w,color_hsvToString:()=>Ue,color_hsvValue:()=>Hn,color_isColorName:()=>l,color_isHsv:()=>Rn,color_isRgb:()=>Fe,color_isRgbComponent:()=>ze,color_rgb:()=>Je,color_rgbAdd:()=>re,color_rgbAlpha:()=>_e,color_rgbAverage:()=>je,color_rgbBlue:()=>it,color_rgbBluer:()=>ti,color_rgbDarker:()=>m,color_rgbDistance:()=>wt,color_rgbGreen:()=>fe,color_rgbGreener:()=>yt,color_rgbGreyscale:()=>kt,color_rgbHue:()=>j,color_rgbLighter:()=>Ee,color_rgbPhaseshift:()=>mr,color_rgbPseudoComplement:()=>Se,color_rgbRed:()=>Rt,color_rgbRedder:()=>Pt,color_rgbRotateComponents:()=>Ut,color_rgbSaturation:()=>$e,color_rgbSubtract:()=>a,color_rgbThicken:()=>_n,color_rgbThin:()=>Ar,color_rgbToColorName:()=>tr,color_rgbToHsv:()=>br,color_rgbToString:()=>Pn,color_rgbValue:()=>Qt,drawing_above:()=>Ne,drawing_aboveAlign:()=>Le,drawing_beside:()=>Ke,drawing_besideAlign:()=>ne,drawing_canvasClass:()=>he,drawing_circle:()=>ke,drawing_circleDiameter:()=>nt,drawing_circleQ:()=>$r,drawing_clearDrawing:()=>f,drawing_diamond:()=>St,drawing_diamondHeight:()=>ue,drawing_diamondQ:()=>_t,drawing_diamondWidth:()=>Dt,drawing_drawingColor:()=>oe,drawing_drawingDescription:()=>we,drawing_drawingHeight:()=>fr,drawing_drawingQ:()=>be,drawing_drawingRecolor:()=>It,drawing_drawingToCanvas:()=>Mt,drawing_drawingToPixels:()=>Vt,drawing_drawingWidth:()=>Ze,drawing_ellipse:()=>r,drawing_ellipseHeight:()=>vn,drawing_ellipseQ:()=>jr,drawing_ellipseWidth:()=>nr,drawing_equilateralTriangle:()=>xr,drawing_equilateralTriangleEdge:()=>Fn,drawing_equilateralTriangleQ:()=>$t,drawing_fillModeQ:()=>or,drawing_isoscelesTriangle:()=>Er,drawing_isoscelesTriangleHeight:()=>ln,drawing_isoscelesTriangleQ:()=>qn,drawing_isoscelesTriangleWidth:()=>Pr,drawing_normalize:()=>mn,drawing_outlinedCircle:()=>Sn,drawing_outlinedDiamond:()=>dr,drawing_outlinedEllipse:()=>Zn,drawing_outlinedEquilateralTriangle:()=>An,drawing_outlinedIsoscelesTriangle:()=>wn,drawing_outlinedPolygon:()=>Jt,drawing_outlinedQ:()=>zr,drawing_outlinedRectangle:()=>Qr,drawing_outlinedRightTriangle:()=>an,drawing_outlinedSquare:()=>qr,drawing_outlinedTriangle:()=>dt,drawing_outlinedWedge:()=>x,drawing_overlay:()=>Ur,drawing_overlayAlign:()=>gt,drawing_overlayOffset:()=>k,drawing_path:()=>E,drawing_polygon:()=>Ge,drawing_polygonPoints:()=>Wn,drawing_polygonQ:()=>d,drawing_rectangle:()=>Bn,drawing_rectangleHeight:()=>Ie,drawing_rectangleQ:()=>Be,drawing_rectangleWidth:()=>Xe,drawing_redescribe:()=>ae,drawing_render:()=>ye,drawing_renderer:()=>Me,drawing_rightTriangle:()=>ot,drawing_rightTriangleHeight:()=>ri,drawing_rightTriangleQ:()=>g,drawing_rightTriangleWidth:()=>Et,drawing_rotate:()=>me,drawing_solidCircle:()=>xt,drawing_solidDiamond:()=>jt,drawing_solidEllipse:()=>le,drawing_solidEquilateralTriangle:()=>Oe,drawing_solidIsoscelesTriangle:()=>gr,drawing_solidPolygon:()=>Ce,drawing_solidQ:()=>Bt,drawing_solidRectangle:()=>Ft,drawing_solidRightTriangle:()=>Wt,drawing_solidSquare:()=>tt,drawing_solidTriangle:()=>o,drawing_solidWedge:()=>gn,drawing_square:()=>kr,drawing_squareQ:()=>er,drawing_squareSide:()=>yr,drawing_text:()=>Nn,drawing_triangle:()=>Zt,drawing_wedge:()=>ir,drawing_wedgeAngle:()=>Tr,drawing_wedgeQ:()=>cn,drawing_wedgeRadius:()=>Kn,drawing_withDash:()=>Nr,font_font:()=>bn,font_fontQ:()=>lr,font_fontToFontString:()=>Xn,image_blockOnFetchImage:()=>un,image_imageLoad:()=>yn,image_imageSave:()=>sr,image_isReactiveImageFile:()=>Jn,image_withImageFile:()=>Tn}),Ll=t({lab_description:()=>Vl,lab_part:()=>zl,lab_problem:()=>Bl,lab_title:()=>Rl});function Rl(e){B();let t=document.createElement(`h1`);return t.innerText=e,t}function zl(e){B();let t=document.createElement(`h2`);return t.innerText=e,t}function Bl(e){B();let t=document.createElement(`h3`);return t.innerText=e,t}function Vl(e){B();let t=document.createElement(`p`),n=document.createElement(`em`);return n.innerText=e,t.appendChild(n),t}function Hl(e){throw new Dn(async()=>{let{getFS:t}=await ii(async()=>{let{getFS:e}=await import(`./fs-43.4.0-rc.1.js`);return{getFS:e}},__vite__mapDeps([0,1,2,3])),n=t();if(!await n.fileExists(e))throw new L(`Runtime`,`File "${e}" does not exist`);return n.loadFile(e)})}function Ul(e){return{[R]:`struct`,[F]:`reactive-file-chooser`,callback:e,[Xt]:z()}}var Wl=t({prelude_abs:()=>hu,prelude_acos:()=>Pu,prelude_append:()=>td,prelude_apply:()=>Kl,prelude_asin:()=>Nu,prelude_assocKey:()=>cd,prelude_assocRef:()=>ld,prelude_assocSet:()=>ud,prelude_atan:()=>Fu,prelude_blockOnReadFile:()=>Hl,prelude_booleanQ:()=>Ru,prelude_car:()=>Ku,prelude_cdr:()=>qu,prelude_ceiling:()=>bu,prelude_charCompareFns:()=>pd,prelude_charDowncase:()=>xd,prelude_charFoldcase:()=>Sd,prelude_charPredicateFns:()=>hd,prelude_charQ:()=>dd,prelude_charToInteger:()=>vd,prelude_charUpcase:()=>bd,prelude_cons:()=>Wu,prelude_cos:()=>ju,prelude_deref:()=>uf,prelude_digitalValue:()=>_d,prelude_div:()=>mu,prelude_elseConst:()=>!0,prelude_eq:()=>ru,prelude_equalQ:()=>Jl,prelude_equalsEps:()=>Iu,prelude_error:()=>Gl,prelude_evenQ:()=>cu,prelude_exp:()=>Ou,prelude_expt:()=>Tu,prelude_floor:()=>yu,prelude_geq:()=>nu,prelude_gt:()=>tu,prelude_hashCount:()=>bf,prelude_hashHasKeyQ:()=>gf,prelude_hashKeys:()=>xf,prelude_hashQ:()=>pf,prelude_hashRef:()=>mf,prelude_hashRefOr:()=>hf,prelude_hashRemove:()=>yf,prelude_hashSet:()=>_f,prelude_hashSetBang:()=>vf,prelude_hashToList:()=>Cf,prelude_hashValues:()=>Sf,prelude_ignore:()=>af,prelude_implies:()=>Vu,prelude_indexOf:()=>sd,prelude_integerQ:()=>Zl,prelude_integerToChar:()=>yd,prelude_isRef:()=>lf,prelude_length:()=>$u,prelude_leq:()=>eu,prelude_list:()=>Zu,prelude_listDrop:()=>ad,prelude_listQ:()=>Yu,prelude_listRef:()=>od,prelude_listTail:()=>rd,prelude_listTake:()=>id,prelude_listToHash:()=>wf,prelude_listToString:()=>Id,prelude_listToVector:()=>Zd,prelude_log:()=>ku,prelude_lt:()=>$l,prelude_makeList:()=>Qu,prelude_makeString:()=>wd,prelude_makeVector:()=>Gd,prelude_max:()=>lu,prelude_min:()=>uu,prelude_minus:()=>fu,prelude_modulo:()=>vu,prelude_nanQ:()=>Ql,prelude_nand:()=>zu,prelude_negativeQ:()=>ou,prelude_nonemptyListQ:()=>Xu,prelude_nor:()=>Bu,prelude_not:()=>Lu,prelude_nullConst:()=>null,prelude_nullQ:()=>Ju,prelude_numberQ:()=>Yl,prelude_numberToString:()=>Eu,prelude_oddQ:()=>su,prelude_pair:()=>Gu,prelude_pairQ:()=>Uu,prelude_piConst:()=>Tf,prelude_plus:()=>du,prelude_positiveQ:()=>au,prelude_procedureQ:()=>ef,prelude_quotient:()=>gu,prelude_random:()=>rf,prelude_range:()=>nf,prelude_realQ:()=>Xl,prelude_ref:()=>cf,prelude_refSet:()=>df,prelude_remainder:()=>_u,prelude_reverse:()=>nd,prelude_round:()=>Su,prelude_setMaximumRecursionDepth:()=>of,prelude_sin:()=>Au,prelude_sqrt:()=>wu,prelude_square:()=>Cu,prelude_string:()=>Td,prelude_stringAppend:()=>Pd,prelude_stringCompareFns:()=>Od,prelude_stringContains:()=>zd,prelude_stringDowncase:()=>jd,prelude_stringFoldcase:()=>Md,prelude_stringLength:()=>Ed,prelude_stringQ:()=>Cd,prelude_stringRef:()=>Dd,prelude_stringSplit:()=>Vd,prelude_stringSplitVector:()=>Hd,prelude_stringToList:()=>Fd,prelude_stringToNumber:()=>Du,prelude_stringToVector:()=>Ld,prelude_stringToWords:()=>sf,prelude_stringUpcase:()=>Ad,prelude_substring:()=>Nd,prelude_tan:()=>Mu,prelude_times:()=>pu,prelude_truncate:()=>xu,prelude_vector:()=>Wd,prelude_vectorAppend:()=>$d,prelude_vectorFill:()=>Yd,prelude_vectorLength:()=>Kd,prelude_vectorQ:()=>Ud,prelude_vectorRange:()=>Qd,prelude_vectorRef:()=>qd,prelude_vectorSet:()=>Jd,prelude_vectorToList:()=>Xd,prelude_vectorToString:()=>Rd,prelude_voidConst:()=>void 0,prelude_voidQ:()=>tf,prelude_withFileChooser:()=>Ul,prelude_withHandler:()=>ql,prelude_xor:()=>Hu,prelude_zeroQ:()=>iu}),Gl=Yt(`error`,e=>{throw typeof e==`string`?new L(`Runtime`,e,void 0,void 0,`error`):new L(`Runtime`,`expected a string, received ${I(e)}`,void 0,void 0,`error`)}),Kl=A([`f`,`args`],[Mn(`f`),Mn(`args`),Re()],[],()=>{throw new H(`prelude_apply`,`apply closure.call must never be invoked`)},`apply`,void 0,`builtin`),ql=A([`handler`,`thunk`],[Mn(`handler`),Mn(`thunk`),Or(),Pe(0),i()],[],()=>{throw new H(`prelude_withHandler`,`closure.call must never be invoked`)},`with-handler`,void 0,`builtin`);function Jl(e,t){return Gt(e,t)}function Yl(e){return typeof e==`number`}function Xl(e){return typeof e==`number`&&Number.isFinite(e)}function Zl(e){return typeof e==`number`&&Number.isInteger(e)}function Ql(e){return Number.isNaN(e)}function $l(e,t){return e<t}function eu(e,t){return e<=t}function tu(e,t){return e>t}function nu(e,t){return e>=t}function ru(e,t){return e===t}function iu(e){return e===0}function au(e){return e>0}function ou(e){return e<0}function su(e){return(e&1)==1}function cu(e){return(e&1)!=1}function lu(...e){return Math.max(...e)}function uu(...e){return Math.min(...e)}function du(...e){return e.reduce((e,t)=>e+t,0)}function fu(...e){return e.length===1?-e[0]:e.reduce((e,t)=>e-t)}function pu(...e){return e.reduce((e,t)=>e*t,1)}function mu(...e){let t=(e,t)=>{if(t===0)throw new L(`Runtime`,`/: division by zero`);return e/t};return e.length===1?t(1,e[0]):e.reduce(t)}function hu(e){return Math.abs(e)}function gu(e,t){if(t===0)throw new L(`Runtime`,`quotient: division by zero`);return Math.trunc(e/t)}function _u(e,t){if(t===0)throw new L(`Runtime`,`remainder: division by zero`);return e%t}function vu(e,t){if(t===0)throw new L(`Runtime`,`modulo: division by zero`);return(e%t+t)%t}function yu(e){return Math.floor(e)}function bu(e){return Math.ceil(e)}function xu(e){return Math.trunc(e)}function Su(e){return Math.round(e)}function Cu(e){return e*e}function wu(e){return Math.sqrt(e)}function Tu(e,t){return e**+t}function Eu(e){return e.toString()}function Du(e){return/^[+-]?\d+$/.test(e)?parseInt(e):/^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/.test(e)?parseFloat(e):!1}function Ou(e){return Math.exp(e)}function ku(e){return Math.log(e)}function Au(e){return Math.sin(e)}function ju(e){return Math.cos(e)}function Mu(e){return Math.tan(e)}function Nu(e){return Math.asin(e)}function Pu(e){return Math.acos(e)}function Fu(e){return Math.atan(e)}function Iu(e){let t=`=-(${e})`,n=function(t,n){return Math.abs(t-n)<=e};return Yt(t,n),n}function Lu(e){return!e}function Ru(e){return typeof e==`boolean`}function zu(...e){return!e.reduce((e,t)=>e&&t,!0)}function Bu(...e){return!e.reduce((e,t)=>e||t,!1)}function Vu(e,t){return!e||t}function Hu(e,t){return e&&!t||!e&&t}function Uu(e){return C(e)}function Wu(e,t){return Ae(e,t)}function Gu(e,t){return Qe(e,t)}function Ku(e){if(C(e))return e.fst;if(c(e,`cons`))return e.head;throw new L(`Runtime`,`car: expected a pair or a non-empty list`)}function qu(e){if(C(e))return e.snd;if(c(e,`cons`))return e.tail;throw new L(`Runtime`,`cdr: expected a pair or a non-empty list`)}function Ju(e){return e===null}function Yu(e){return pt(e)}function Xu(e){return Yu(e)&&e!==null}function Zu(...e){let t=null;for(let n=e.length-1;n>=0;n--)t=Ae(e[n],t);return t}function Qu(e,t){let n=null;for(let r=0;r<e;r++)n=Ae(t,n);return n}function $u(e){let t=0;for(;e!==null;)t+=1,e=e.tail;return t}function ed(e,t){if(e===null)return t;{let n=Ae(e.head,null),r=n,i=e.tail;for(;i!==null;)r.tail=Ae(i.head,null),r=r.tail,i=i.tail;return r.tail=t,n}}function td(...e){let t=null;for(let n of e)t=ed(t,n);return t}function nd(e){let t=[];for(;e!==null;)t.push(e),e=e.tail;t.reverse();let n=null;for(;t.length>0;){let e=Gn(t,`the list being built`);n=Ae(e.head,n)}return n}function rd(e,t){for(;e!==null&&t>0;)e=e.tail,--t;return e}function id(e,t){let n=[];for(;e!==null&&t>0;)n.push(e.head),e=e.tail,--t;let r=null;for(let e=n.length-1;e>=0;e--)r=Ae(n[e],r);return r}function ad(e,t){for(;e!==null&&t>0;)e=e.tail,--t;return e}function od(e,t){let n=t;for(;e!==null&&n>0;)e=e.tail,--n;if(e===null)throw new L(`Runtime`,`list-ref: index ${t} out of bounds of list`);return e.head}function sd(e,t){let n=0;for(;t!==null;){if(Gt(t.head,e))return n;t=t.tail,n+=1}return-1}function cd(e,t){for(;t!==null;){if(Gt(t.head.fst,e))return!0;t=t.tail}return!1}function ld(e,t){for(;t!==null;){if(Gt(t.head.fst,e))return t.head.snd;t=t.tail}throw new L(`Runtime`,`assoc-ref: key ${sn.render(e)} not found in association list`)}function ud(e,t,n){let r=[];for(;n!==null;){let i=n.head;if(Gt(i.fst,e)){r.push(Qe(e,t));let i=n.tail;for(let e=r.length-1;e>=0;e--)i=Ae(r[e],i);return i}r.push(n.head),n=n.tail}return V(r.concat([Qe(e,t)]))}function dd(e){return Gr(e)}function fd(e,t){if(t.length<=1)return!0;for(let n=0;n<t.length-1;n++)if(!e(t[n],t[n+1]))return!1;return!0}var pd={};function md(e,t){let n=function(...e){return fd((e,n)=>t(e.value,n.value),e)};Yt(e,n),pd[`prelude_${e}`]=n}md(`char=?`,(e,t)=>e===t),md(`char<?`,(e,t)=>e<t),md(`char>?`,(e,t)=>e>t),md(`char<=?`,(e,t)=>e<=t),md(`char>=?`,(e,t)=>e>=t),md(`char-ci=?`,(e,t)=>e.toLowerCase()===t.toLowerCase()),md(`char-ci<?`,(e,t)=>e.toLowerCase()<t.toLowerCase()),md(`char-ci>?`,(e,t)=>e.toLowerCase()>t.toLowerCase()),md(`char-ci<=?`,(e,t)=>e.toLowerCase()<=t.toLowerCase()),md(`char-ci>=?`,(e,t)=>e.toLowerCase()>=t.toLowerCase());var hd={};function gd(e,t){let n=function(e){return t(e.value)};Yt(e,n),hd[`prelude_${e}`]=n}gd(`char-alphabetic?`,e=>/\p{L}/gu.test(e)),gd(`char-numeric?`,e=>/\p{N}/gu.test(e)),gd(`char-whitespace?`,e=>/\p{Z}/gu.test(e)),gd(`char-upper-case?`,e=>/\p{Lu}/gu.test(e)),gd(`char-lower-case?`,e=>/\p{Ll}/gu.test(e));function _d(e){let t=parseInt(e.value,10);if(isNaN(t))throw new L(`Runtime`,`digit-value: ${e.value} is not a decimal digit`);return t}function vd(e){let t=e.value.codePointAt(0);if(t===void 0)throw new H(`prelude_charToInteger`,`a character with no code point`);return t}function yd(e){return qe(String.fromCodePoint(e))}function bd(e){return qe(e.value.toUpperCase())}function xd(e){return qe(e.value.toLowerCase())}function Sd(e){return qe(e.value.toLowerCase())}function Cd(e){return typeof e==`string`}function wd(e,t){return t.value.repeat(e)}function Td(...e){return e.map(e=>e.value).join(``)}function Ed(e){return e.length}function Dd(e,t){return qe(e[t])}var Od={};function kd(e,t){let n=function(...e){return fd((e,n)=>t(e,n),e)};Yt(e,n),Od[`prelude_${e}`]=n}kd(`string=?`,(e,t)=>e===t),kd(`string<?`,(e,t)=>e<t),kd(`string>?`,(e,t)=>e>t),kd(`string<=?`,(e,t)=>e<=t),kd(`string>=?`,(e,t)=>e>=t),kd(`string-ci=?`,(e,t)=>e.toLowerCase()===t.toLowerCase()),kd(`string-ci<?`,(e,t)=>e.toLowerCase()<t.toLowerCase()),kd(`string-ci>?`,(e,t)=>e.toLowerCase()>t.toLowerCase()),kd(`string-ci<=?`,(e,t)=>e.toLowerCase()<=t.toLowerCase()),kd(`string-ci>=?`,(e,t)=>e.toLowerCase()>=t.toLowerCase());function Ad(e){return e.toUpperCase()}function jd(e){return e.toLowerCase()}function Md(e){return e.toLowerCase()}function Nd(e,t,n){return e.substring(t,n)}function Pd(...e){return e.join(``)}function Fd(e){let t=null;for(let n=e.length-1;n>=0;n--)t=Ae(qe(e[n]),t);return t}function Id(e){let t=``;for(;e!==null;){if(!Gr(e.head))throw new L(`Runtime`,`list->string: list contains non-character element: ${I(e.head)}`);t+=e.head.value,e=e.tail}return t}function Ld(e){let t=[];for(let n=0;n<e.length;n++)t.push(qe(e[n]));return t}function Rd(e){let t=``;for(let n of e)t+=n.value;return t}function zd(e,t){return e.includes(t)}function Bd(e,t){if(t===``)return e.split(t);let n=e.startsWith(t)?t.length:0,r=e.endsWith(t)?Math.max(n,e.length-t.length):e.length,i=e.slice(n,r);return i===``?[]:i.split(t)}function Vd(e,t){let n=Bd(e,t),r=null;for(let e=n.length-1;e>=0;e--)r=Ae(n[e],r);return r}function Hd(e,t){return Bd(e,t)}function Ud(e){return nn(e)}function Wd(...e){return e}function Gd(e,t){let n=[];for(let r=0;r<e;r++)n.push(t);return n}function Kd(e){return e.length}function qd(e,t){if(t<0||t>=e.length)throw new L(`Runtime`,`vector-ref: index ${t} out of bounds of vector`);return e[t]}function Jd(e,t,n){if(t<0||t>=e.length)throw new L(`Runtime`,`vector-set!: index ${t} out of bounds of vector`);e[t]=n}function Yd(e,t){for(let n=0;n<e.length;n++)e[n]=t}function Xd(e){let t=null;for(let n=e.length-1;n>=0;n--)t=Ae(e[n],t);return t}function Zd(e){let t=[];for(;e!==null;)t.push(e.head),e=e.tail;return t}function Qd(...e){if(e.length===0||e.length>3)throw new L(`Runtime`,`1, 2, or 3 numbers must be passed to function`);{let t=e.length===1?0:e[0],n=e.length===1?e[0]:e[1],r=e.length<3?1:e[2],i=[];if(r===0)throw new L(`Runtime`,`"step" argument must be non-zero`);for(let e=t;r>0?e<n:e>n;e+=r)i.push(e);return i}}function $d(...e){let t=[];for(let n of e)for(let e of n)t.push(e);return t}function ef(e){return ct(e)||Vr(e)}function tf(e){return e===void 0}function nf(...e){if(e.length===0||e.length>3)throw new L(`Runtime`,`1, 2, or 3 numbers must be passed to function`);{let t=e.length===1?0:e[0],n=e.length===1?e[0]:e[1],r=e.length<3?1:e[2],i=[];if(r===0)throw new L(`Runtime`,`"step" argument must be non-zero`);for(let e=t;r>0?e<n:e>n;e+=r)i.push(e);return V(i)}}function rf(e){return Math.floor(Math.random()*e)}function af(e){B();let t=document.createElement(`div`);return t.style.display=`non`,t}function of(e){throw!Number.isInteger(e)||e<1||e>2e5?new L(`Runtime`,`expects a whole number between 1 and ${oi.toString()}, but was given ${e.toString()}`):new cr(e)}function sf(e){let t=e.split(/\s+/);for(let e=0;e<t.length;e++)t[e]=t[e].replace(/[.,;?:!]$/,``);return V(t.filter(e=>e.length>0))}function cf(e){return{[R]:`struct`,[F]:`ref`,value:e}}function lf(e){return c(e,`ref`)}function uf(e){return e.value}function df(e,t){e.value=t}function ff(e,t){if(typeof t!=`string`)throw new L(`Runtime`,`${e}: a map key must be a string, received ${I(t)}`);return t}function pf(e){return te(e)}function mf(e,t){let n=ff(`hash-ref`,t);if(!Object.prototype.hasOwnProperty.call(e,n))throw new L(`Runtime`,`hash-ref: no value for key "${n}"`);return e[n]}function hf(e,t,n){let r=ff(`hash-ref-or`,t);return Object.prototype.hasOwnProperty.call(e,r)?e[r]:n}function gf(e,t){return Object.prototype.hasOwnProperty.call(e,ff(`hash-has-key?`,t))}function _f(e,t,n){return{...e,[ff(`hash-set`,t)]:n}}function vf(e,t,n){e[ff(`hash-set!`,t)]=n}function yf(e,t){let n=ff(`hash-remove`,t),r={};for(let t of Object.keys(e))t!==n&&(r[t]=e[t]);return r}function bf(e){return Object.keys(e).length}function xf(e){return V(Object.keys(e))}function Sf(e){return V(Object.keys(e).map(t=>e[t]))}function Cf(e){return V(Object.keys(e).map(t=>Qe(t,e[t])))}function wf(e){let t={};for(let n of Ln(e)){if(!C(n))throw new L(`Runtime`,`list->hash: expected a list of pairs, but the list contains ${I(n)}`);t[ff(`list->hash`,n.fst)]=n.snd}return t}var Tf=Math.PI,Ef=t({reactive_onButtonClick:()=>Nf,reactive_onKeyDown:()=>If,reactive_onKeyUp:()=>Lf,reactive_onMouseClick:()=>Pf,reactive_onMouseHover:()=>Ff,reactive_onNote:()=>zf,reactive_onTimer:()=>Rf,reactive_reactiveCanvas:()=>kf,reactive_reactiveContainer:()=>jf,reactive_subscriptionQ:()=>Df});function Df(e){return c(e,`subscription`)}var Of=class{run=z();canvas;state;viewFunc;updateFunc;isDirty;finished;queue=[];updating=!1;drawing=!1;constructor(e,t,n,r,i){this.canvas=document.createElement(`canvas`),this.canvas.width=e,this.canvas.height=t,this.state=n,this.viewFunc=r,this.updateFunc=i,this.isDirty=!0,this.finished=!1,this.run.signal?.addEventListener(`abort`,()=>{this.finished=!0});let a=e=>{this.draw(),this.finished||requestAnimationFrame(a)};requestAnimationFrame(a)}getState(){return this.state}draw(){this.finished||this.drawing||!this.isDirty||(this.drawing=!0,this.isDirty=!1,pn(this.canvas).clearRect(0,0,this.canvas.width,this.canvas.height),this.run.spawn(this.viewFunc,[this.state,this.canvas],e=>{this.drawing=!1,e===null&&(this.finished=!0)}))}update(e){this.finished||(this.queue.push(e),this.processQueue())}processQueue(){if(this.updating||this.finished||this.queue.length===0)return;this.updating=!0;let e=Mr(this.queue,`the message queue`);this.run.spawn(this.updateFunc,[e,this.state],e=>{e===null?this.finished=!0:(this.state=e,this.isDirty=!0),this.updating=!1,this.processQueue()})}getElement(){return this.canvas}};function kf(e,t,n,r,i,...a){B();let o=new Of(e,t,n,r,i);return a.forEach(e=>{e.register(o)}),o.getElement()}var Af=class{run=z();container;state;viewFunc;updateFunc;finished;queue=[];processing=!1;constructor(e,t,n){this.container=document.createElement(`div`),this.state=e,this.viewFunc=t,this.updateFunc=n,this.finished=!1,this.run.signal?.addEventListener(`abort`,()=>{this.finished=!0})}renderView(e){this.run.spawn(this.viewFunc,[this.state],t=>{t instanceof HTMLElement&&(this.container.innerHTML=``,this.container.appendChild(t)),e?.()})}draw(){this.renderView()}update(e){this.finished||(this.queue.push(e),this.processQueue())}processQueue(){if(this.processing||this.finished||this.queue.length===0)return;this.processing=!0;let e=Mr(this.queue,`the message queue`);this.run.spawn(this.updateFunc,[e,this.state],e=>{if(e===null){this.finished=!0,this.processing=!1;return}this.state=e,this.renderView(()=>{this.processing=!1,this.processQueue()})})}getElement(){return this.container}};function jf(e,t,n,...r){B();let i=new Af(e,t,n);return r.forEach(e=>{e.register(i)}),i.draw(),i.getElement()}function Mf(e){return{[R]:`struct`,[F]:`subscription`,register:e}}function Nf(e){let t=z();return Mf(n=>{e.addEventListener(`click`,()=>{n.update({[R]:`struct`,[F]:`event-button-click`,id:e.id})},{signal:t.signal})})}function Pf(){let e=z();return Mf(t=>{t.getElement().addEventListener(`click`,e=>{let n=t.getElement().getBoundingClientRect();t.update({[R]:`struct`,[F]:`event-mouse-click`,button:e.button,x:e.clientX-n.left,y:e.clientY-n.top})},{signal:e.signal})})}function Ff(){let e=z();return Mf(t=>{t.getElement().addEventListener(`mousemove`,e=>{let n=t.getElement().getBoundingClientRect();t.update({[R]:`struct`,[F]:`event-mouse-hover`,x:e.clientX-n.left,y:e.clientY-n.top})},{signal:e.signal})})}function If(){let e=z();return Mf(t=>{document.addEventListener(`keydown`,e=>{t.update({[R]:`struct`,[F]:`event-key-down`,key:e.key})},{signal:e.signal})})}function Lf(){let e=z();return Mf(t=>{document.addEventListener(`keyup`,e=>{t.update({[R]:`struct`,[F]:`event-key-up`,key:e.key})},{signal:e.signal})})}function Rf(e){let t=z();return Mf(n=>{let r=performance.now(),i=t.signal,a=setInterval(()=>{let e=performance.now();n.update({[R]:`struct`,[F]:`event-timer`,time:e,elapsed:e-r}),r=e},e);i?.addEventListener(`abort`,()=>{clearInterval(a)})})}function zf(e){return Mf(t=>{e.push(e=>{t.update(e)})})}var Bf=t({rex_isRegex:()=>tp,rex_rexAnyChar:()=>sp,rex_rexAnyOf:()=>dp,rex_rexCharAntiset:()=>lp,rex_rexCharRange:()=>up,rex_rexCharSet:()=>cp,rex_rexConcat:()=>op,rex_rexEmpty:()=>np,rex_rexFindMatches:()=>mp,rex_rexMatches:()=>hp,rex_rexOptional:()=>fp,rex_rexRegex:()=>pp,rex_rexRepeat:()=>ip,rex_rexRepeat0:()=>ap,rex_rexSplitString:()=>gp,rex_rexString:()=>rp,rex_rexToString:()=>_p}),Vf=RegExp(`(\\`+[`/`,`.`,`*`,`+`,`?`,`|`,`(`,`)`,`[`,`]`,`{`,`}`,`\\`].join(`|\\`)+`)`,`g`);function Hf(e){return e.replace(Vf,`\\$1`)}var Uf=class{[R]=`struct`;[F]=`rex-empty`;toRegexString(){return``}},Wf=class{[R]=`struct`;[F]=`rex-string`;value;constructor(e){this.value=e}toRegexString(){return Hf(this.value)}},Gf=class{[R]=`struct`;[F]=`rex-repeat`;value;constructor(e){this.value=e}toRegexString(){return`(?:${this.value.toRegexString()})+`}},Kf=class{[R]=`struct`;[F]=`rex-repeat-0`;value;constructor(e){this.value=e}toRegexString(){return`(?:${this.value.toRegexString()})*`}},qf=class{[R]=`struct`;[F]=`rex-concat`;values;constructor(e){this.values=e}toRegexString(){return this.values.map(e=>e.toRegexString()).join(``)}},Jf=class{[R]=`struct`;[F]=`rex-any-char`;toRegexString(){return`.`}},Yf=class{[R]=`struct`;[F]=`rex-char-set`;chars;constructor(e){this.chars=e}toRegexString(){return`[${Hf(this.chars)}]`}},Xf=class{[R]=`struct`;[F]=`rex-char-antiset`;chars;constructor(e){this.chars=e}toRegexString(){return`[^${Hf(this.chars)}]`}},Zf=class{[R]=`struct`;[F]=`rex-char-range`;start;end;constructor(e,t){this.start=e,this.end=t}toRegexString(){return`[${Hf(this.start.value)}-${Hf(this.end.value)}]`}},Qf=class{[R]=`struct`;[F]=`rex-any-of`;values;constructor(e){this.values=e}toRegexString(){return`(?:${this.values.map(e=>e.toRegexString()).join(`|`)})`}},$f=class{[R]=`struct`;[F]=`rex-optional`;value;constructor(e){this.value=e}toRegexString(){return`(?:${this.value.toRegexString()})?`}},ep=class{[R]=`struct`;[F]=`regex`;pattern;constructor(e){this.pattern=e}toRegexString(){return this.pattern}};function tp(e){return c(e,`rex-empty`)||c(e,`rex-string`)||c(e,`rex-repeat`)||c(e,`rex-repeat-0`)||c(e,`rex-concat`)||c(e,`rex-any-char`)||c(e,`rex-char-set`)||c(e,`rex-char-antiset`)||c(e,`rex-char-range`)||c(e,`rex-any-of`)||c(e,`rex-optional`)||c(e,`regex`)}function np(){return new Uf}function rp(e){return new Wf(e)}function ip(e){return new Gf(e)}function ap(e){return new Kf(e)}function op(...e){return new qf(e)}function sp(){return new Jf}function cp(e){return new Yf(e)}function lp(e){return new Xf(e)}function up(e,t){return new Zf(e,t)}function dp(...e){return new Qf(e)}function fp(e){return new $f(e)}function pp(e){return new ep(e)}function mp(e,t){let n=new RegExp(e.toRegexString(),`g`),r=[],i;for(;(i=n.exec(t))!==null;)r.push(i[0]),i.index===n.lastIndex&&n.lastIndex++;return V(r)}function hp(e,t){return RegExp(`^${e.toRegexString()}$`).test(t)}function gp(e,t){let n=new RegExp(e.toRegexString(),`g`),r=t.split(n);return V(r)}function _p(e){return e.toRegexString()}var vp=t({runtime_any:()=>jp,runtime_checkArity:()=>wp,runtime_contracted:()=>Ap,runtime_error:()=>bp,runtime_mkCtorFn:()=>Dp,runtime_mkGetFn:()=>Op,runtime_mkObj:()=>Np,runtime_mkPredFn:()=>Ep,runtime_mkVec:()=>Mp,runtime_optArg:()=>Sp,runtime_optRest:()=>Cp,runtime_report:()=>yp,runtime_typeOf:()=>kp,runtime_voidQ:()=>Tp});function yp(e){throw new fn(e,Yr.none)}var bp=Yt(`error`,e=>{throw typeof e==`string`?new L(`Runtime`,e,void 0,void 0,`error`):new L(`Runtime`,`expected a string, received ${I(e)}`,void 0,void 0,`error`)});function xp(e,t){let n=e;for(let e=0;e<t&&n!==null;e++)n=n.tail;return n}function Sp(e,t){let n=xp(e,t);return n===null?void 0:n.head}function Cp(e,t){return xp(e,t)}function wp(e,t,n){let r=xp(e,t);if(r===null)return;let i=n+t;for(;r!==null;)i+=1,r=r.tail;throw new L(`Runtime`,`Arity mismatch in function call: expected at most ${n+t} arguments, got ${i}`)}function Tp(e){return e===void 0}function Ep(e){return t=>c(t,e)}function Dp(e,t){return(...n)=>{if(n.length!==t.length)throw new L(`Runtime`,`Constructor ${e} expects ${t.length} arguments, received ${n.length}`);return vr(e,t,n)}}function Op(e,t){return n=>{if(c(n,e)){if(!(t in n))throw new L(`Runtime`,`Accessor expects field ${t} but it is not present in the given struct value`);return n[t]}throw new L(`Runtime`,`Accessor function expects a ${e}, received ${I(n)}`)}}function kp(e){return I(e)}function Ap(e,t){return ct(t)?{...t,contractTarget:e}:t}function jp(e){return!0}function Mp(...e){return e}function Np(...e){if(e.length%2!=0)throw new L(`Runtime`,`A map requires an even number of arguments (alternating keys and values), received ${e.length.toString()}`);let t={};for(let n=0;n<e.length;n+=2){let r=e[n];if(typeof r!=`string`)throw new L(`Runtime`,`A map key must be a string, received ${I(r)}`);t[r]=e[n+1]}return t}sn.registerCustomRenderer(Qn,_r),sn.registerCustomRenderer(Dr,_r);var{prelude_charCompareFns:Pp,prelude_charPredicateFns:Fp,prelude_stringCompareFns:Ip,...Lp}=Wl,Rp=new Map([...Object.entries(ar),...Object.entries(Kc),...Object.entries(gl),...Object.entries(_l),...Object.entries(El),...Object.entries(Il),...Object.entries(Ll),...Object.entries(Sr),...Object.entries(Lp),...Object.entries(Pp),...Object.entries(Fp),...Object.entries(Ip),...Object.entries(Ef),...Object.entries(Bf),...Object.entries(vp),...Object.entries(jn)]);function zp(e){if(!Rp.has(e))throw new L(`Runtime`,`Attempted to look up "${e}" but it is not bound!`);return Rp.get(e)}var Bp=Yt(`js-var`,e=>zp(e));function Vp(e){let t=[0];for(let n=0;n<e.length;n++)e[n]===`
`&&t.push(n+1);return t}function Hp(e,t){let n=0,r=t.length-1;for(;n<r;){let i=n+r+1>>1;t[i]<=e?n=i:r=i-1}return new Ir(n+1,e-t[n]+1,e)}function Up(e){let t=Vp(e),n=[],r=bs.parse(e).cursor();do if(r.name===`LineComment`){let{from:i,to:a}=r;n.push({line:e.slice(i,a).trimEnd(),range:new Yr(Hp(i,t),Hp(Math.max(i,a-1),t))})}while(r.next());return n}function Wp(e){switch(e.tag){case`prog`:return e.body;case`define`:return[e.name,e.value];case`defexport`:return[e.name,e.value];case`export`:return e.names;case`display`:return[e.value];case`stmtexp`:return[e.expr];case`struct`:return[e.name,...e.fields];case`app`:return[e.head,...e.args];case`lam`:return[...e.params,...e.restParam?[e.restParam]:[],e.body];case`let`:return[...e.bindings.flatMap(e=>[e.pat,e.value]),e.body];case`begin`:case`and`:case`or`:return e.exps;case`anonfn`:return[e.body];case`if`:return[e.guard,e.ifB,e.elseB];case`match`:return[e.scrutinee,...e.branches.flatMap(e=>[e.pat,e.body])];case`cond`:return e.branches.flatMap(e=>[e.test,e.body]);case`pctor`:return[e.name,...e.args];case`pvec`:return e.args;case`vec`:return e.exps;case`obj`:return e.pairs.flatMap(e=>[e.key,e.value]);case`import`:case`lit`:case`id`:case`hole`:case`pwild`:case`plit`:return[]}}function Gp(e,t){return e?[...e,...t]:[...t]}function Kp(e,t){if(t.length===0)return;let n=Wp(e);if(n.length===0){e.dangling=Gp(e.dangling,t);return}let r=0,i=[],a=null,o=e=>{a!==null&&a.range.end.line===e.range.begin.line?a.trailing=Gp(a.trailing,[e]):i.push(e)};for(let e of n){for(;r<t.length&&t[r].range.end.idx<e.range.begin.idx;)o(t[r++]);i.length>0&&(e.leading=Gp(e.leading,i),i=[]);let n=[];for(;r<t.length&&t[r].range.end.idx<=e.range.end.idx;)n.push(t[r++]);n.length>0&&Kp(e,n),a=e}for(;r<t.length;)o(t[r++]);i.length>0&&(e.dangling=Gp(e.dangling,i))}function qp(e,t){t.length!==0&&Kp(e,[...t].sort((e,t)=>e.range.begin.idx-t.range.begin.idx))}function Jp(e){let t=[],n=[];for(let r of e){let e=n.at(-1);e!==void 0&&r.range.begin.line===e.range.begin.line+1?n.push(r):(n.length>0&&t.push(n),n=[r])}return n.length>0&&t.push(n),t}function Yp(e,t){let n=new Set(t.filter(e=>e.tag===`define`||e.tag===`defexport`||e.tag===`struct`).map(e=>e.range.begin.line)),r=new Set;for(let e of t)for(let t=e.range.begin.line;t<=e.range.end.line;t++)r.add(t);let i=Up(e).filter(e=>!r.has(e.range.begin.line));for(let e of Jp(i))if(!n.has(e[e.length-1].range.begin.line+1)&&Xa(e).length>0)return e}function Xp(e){let t=Xa(e);if(t.length===0)return;let n=t.map(e=>e.line).join(` `).trim();return n.length===0?void 0:{description:n}}function Zp(e,t){let n=Yp(e,t);return n===void 0?void 0:Xp(n)}var Qp=[[`audio`,`;;; The Scamper audio processing library

;;; (sample? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an audio sample.
(define-export sample? (js-var "audio_sampleQ"))

;;; (sample-node v) -> sample?
;;;  v : any
;;;   vector? of numbers between -1.0 and 1.0
;;; Returns an audio sample generated from the provided example.
;;; @category audio, sound
(define-export sample-node (js-var "audio_sampleNode"))

;;; (context? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an audio context.
(define-export context? (js-var "audio_contextQ"))

;;; (audio-context sampleRate) -> context?
;;;  sampleRate : integer?
;;;   positive
;;; Creates an audio context with the given sample rate.
;;; @category audio, sound
(define-export audio-context (js-var "audio_audioContext"))

;;; (audio-node? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an audio node.
(define-export audio-node? (js-var "audio_audioNodeQ"))

;;; (pipeline? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an audio pipeline.
(define-export pipeline? (js-var "audio_pipelineQ"))

;;; (audio-pipeline ctx pipeline & n1) -> pipeline?
;;;  ctx : context?
;;;  pipeline : (or/p audio-node? sample?)
;;;  n1 : (or/p audio-node? sample?)
;;; Creates an audio pipeline from the given audio nodes, connecting the nodes in sequence.
;;; The first argument is what the pipeline plays and may be a sample; the rest are what
;;; it plays through, and must be audio nodes. A sample is played at \`ctx\`'s sample rate,
;;; one frame per element, so the context decides its pitch and duration.
;;; @category audio, sound
(define-export audio-pipeline (js-var "audio_audioPipeline"))

;;; (oscillator-node ctx type freq) -> audio-node?
;;;  ctx : context?
;;;  type : string?
;;;  freq : number?
;;;   positive
;;; Creates an oscillator node with the given type and frequency.
;;; @category audio, sound
(define-export oscillator-node (js-var "audio_oscillatorNode"))

;;; (audio-file-node ctx path) -> audio-node?
;;;  ctx : context?
;;;  path : string?
;;; Creates an audio source node connected to the audio file at the given path.
;;; @category audio, sound
(define-export audio-file-node (js-var "audio_audioFileNode"))

;;; (delay-node ctx delay) -> audio-node?
;;;  ctx : context?
;;;  delay : number?
;;;   positive
;;; Creates a delay node with the given delay time.
;;; @category audio, sound
(define-export delay-node (js-var "audio_delayNode"))

;;; (play-sample sample) -> void?
;;;  sample : sample?
;;; Plays the given audio sample. Note that due to browser limitations, the call to this function must be guarded by user input, _e.g._, by invoking it with a button press.
;;; @category audio, sound
(define-export play-sample (js-var "audio_playSample"))
`],[`canvas`,`;;; Functions for manipulating HTML canvases in Scamper

;;; (canvas? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a canvas.
;;; @category canvas, image, typecheck, make-canvas
(define-export canvas? (js-var "canvas_canvasQ"))

;;; (color? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a valid color: a string containing a named color, an \`rgb\` value, or an \`hsv\` value.
;;; @category color, hsv, image, predicates, rgb, typecheck
(define-export color? (js-var "color_colorQ"))

;;; (drawing? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a drawing: the kind of value the shape constructors build.
;;; @category image, predicates, typecheck
(define-export drawing? (js-var "drawing_drawingQ"))

;;; (fill-mode? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a fill mode: the string \`"solid"\` or \`"outline"\`.
;;; @category canvas, shapes, typecheck, predicates
;; N.B., re-exported here (like color? and drawing?) because this module's own
;; contracts name it, and a contract predicate must resolve in the module that
;; uses it -- see the cross-module predicate test in test/libs/canvas.test.ts.
(define-export fill-mode? (js-var "drawing_fillModeQ"))

;;; (font? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a font.
;;; @category image, typecheck, font, text
;; N.B., re-exported here (like color? and drawing?) because \`canvas-text!\`'s
;; contract names it, and a contract predicate must resolve in the module that
;; uses it.
(define-export font? (js-var "font_fontQ"))

;;; (make-canvas width height) -> canvas?
;;;  width : integer?
;;;   positive
;;;  height : integer?
;;;   positive
;;; Creates a canvas with the given width and height.
;;; @category canvas, canvas?
(define-export make-canvas (js-var "canvas_makeCanvas"))

;;; (canvas-rectangle! canvas x y width height mode color) -> void?
;;;  canvas : canvas?
;;;  x : integer?
;;;  y : integer?
;;;  width : integer?
;;;   non-negative
;;;  height : integer?
;;;   non-negative
;;;  mode : fill-mode?
;;;   either \`"solid"\` or \`"outline"\`
;;;  color : color?
;;; Renders a rectangle whose upper-left corner is at \`(x, y)\`.
;;; @category canvas, mutation, predicates, shapes, canvas-ellipse!, canvas-circle!
(define-export canvas-rectangle! (js-var "canvas_canvasRectangle"))

;;; (canvas-ellipse! canvas x y radiusX radiusY rotation startAngle endAngle mode color) -> void?
;;;  canvas : canvas?
;;;  x : number?
;;;  y : number?
;;;  radiusX : number?
;;;   non-negative
;;;  radiusY : number?
;;;   non-negative
;;;  rotation : number?
;;;  startAngle : number?
;;;  endAngle : number?
;;;  mode : fill-mode?
;;;   either \`"solid"\` or \`"outline"\`
;;;  color : color?
;;; Renders an ellipse whose center is at \`(x, y)\`, radii \`radiusX\` and \`radiusY\`, \`rotation\`, \`startAngle\`, and \`endAngle\`.
;;; @category canvas, mutation, predicates, shapes, canvas-rectangle!, canvas-circle!
(define-export canvas-ellipse! (js-var "canvas_canvasEllipse"))

;;; (canvas-circle! canvas x y radius mode color) -> void?
;;;  canvas : canvas?
;;;  x : number?
;;;  y : number?
;;;  radius : number?
;;;   non-negative
;;;  mode : fill-mode?
;;;   either \`"solid"\` or \`"outline"\`
;;;  color : color?
;;; Renders a circle whose center is at \`(x, y)\` and radius \`radius\`. Note that
;;; this is a radius, where \`circle\` in the image library takes a diameter: a
;;; canvas shape is placed by its center rather than fitted to a box.
;;; @category canvas, mutation, predicates, shapes, canvas-rectangle!, canvas-ellipse!
(define-export canvas-circle! (js-var "canvas_canvasCircle"))

;;; (canvas-text! canvas x y text size mode color [font]) -> void?
;;;  canvas : canvas?
;;;  x : integer?
;;;  y : integer?
;;;  text : string?
;;;  size : number?
;;;   positive
;;;  mode : fill-mode?
;;;   either \`"solid"\` or \`"outline"\`
;;;  color : color?
;;;  font : font?
;;;   defaults to (font "Arial")
;;; Renders the given text at the given coordinates.
;;; @category canvas, mutation, predicates, canvas-drawing!, canvas-path!
(define-export canvas-text! (js-var "canvas_canvasText"))

;;; (canvas-drawing! canvas x y drawing) -> void?
;;;  canvas : canvas?
;;;  x : integer?
;;;  y : integer?
;;;  drawing : drawing?
;;; Draws the given drawing (created via the \`image\` library) at the given coordinates.
;;; @category canvas, mutation, predicates, canvas-text!, canvas-path!
(define-export canvas-drawing! (js-var "canvas_canvasDrawing"))

;;; (canvas-path! canvas pairs mode color) -> void?
;;;  canvas : canvas?
;;;  pairs : list?
;;;   a list of pairs of numbers
;;;  mode : fill-mode?
;;;   either \`"solid"\` or \`"outline"\`
;;;  color : color?
;;; Renders a path from the given list of pairs of numbers.
;;; @category canvas, mutation, path, predicates, canvas-text!, canvas-drawing!
(define-export canvas-path! (js-var "canvas_canvasPath"))

;;; (animate-with proc) -> void?
;;;  proc : procedure?
;;;   a procedure that takes the current time in milliseconds as input.
;;; Repeatedly calls \`proc\` approximately once every 60 seconds, creating the effect of animation. \`proc\` should return a boolean. If \`proc\` returns \`#t\` the loop of calls continues, otherwise, it stops.
;;; @category canvas, canvas-onclick!
(define-export animate-with (js-var "canvas_animateWith"))

;;; (canvas-onclick! canvas proc) -> void?
;;;  canvas : canvas?
;;;  proc : procedure?
;;;   a procedure that takes two arguments: numbers representing the x and y coordinate of the mouse click on the canvas.
;;; Sets the given procedure to be called when the canvas is clicked by the user.
;;; @category canvas, mutation, predicates, animate-with-proc
(define-export canvas-onclick! (js-var "canvas_canvasOnclick"))
`],[`data`,`;;; Data processing and visualization functionality

;;; (parse-csv data) -> list?
;;;  data : string?
;;; Parses \`data\` as a CSV-formatted string and returns a list of rows where each row is a list of fields as strings.
;;; @category data, parse
(define-export parse-csv (js-var "data_parseCsv"))

;;; (string->chars s) -> list?
;;;  s : string?
;;; Converts the string \`s\` into a list of char values.
;;; @category data, string->lines, string->words, tally-all
(define-export string->chars (js-var "data_stringToChars"))

;;; (string->lines s) -> list?
;;;  s : string?
;;; Splits the string \`s\` into a list of strings, where each string is a line of text.
;;; @category data, string->chars, string->words, tally-all
(define-export string->lines (js-var "data_stringToLines"))

;;; (tally-all lst) -> list?
;;;  lst : list?
;;; Takes a list \`lst\` and returns a list of pairs where each pair consists of an element from \`lst\` and the number of times that element appears in \`lst\`.
;;; @category data, string->chars, string->lines
(define-export tally-all (js-var "data_tallyAll"))

;;; (dataset? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if \`v\` is a dataset, \`#f\` otherwise.
;;; @category data, create, plot?
(define-export dataset? (js-var "data_datasetQ"))

;;; (plot? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if \`v\` is a plot, \`#f\` otherwise.
;;; @category data, create, plot, dataset?
(define-export plot? (js-var "data_plotQ"))

;;; (with-plot-options options plot) -> plot?
;;;  options : any
;;;   list of key-value pairs
;;;  plot : plot?
;;; Takes an association list of options and a plot, and returns a new plot with the specified options applied. Valid options include: - 'x-min': sets the minimum x-axis value - 'x-max': sets the maximum x-axis value - 'y-min': sets the minimum y-axis value - 'y-max': sets the maximum y-axis value - 'x-label': sets the label for the x-axis - 'y-label': sets the label for the y-axis
;;; @category data, create, plot, with-dataset-options
(define-export with-plot-options (js-var "data_withPlotOptions"))

;;; (with-dataset-options options dataset) -> dataset?
;;;  options : any
;;;   list of key-value pairs
;;;  dataset : dataset?
;;; Takes an association list of options and a dataset, and returns a new dataset with the specified options applied. Valid options include: - 'background-color': sets the background color of the dataset - 'border-color': sets the border color of the dataset
;;; @category data, create, with-plot-options
(define-export with-dataset-options (js-var "data_withDatasetOptions"))

;;; (plot-linear & datasets) -> plot?
;;;  datasets : any
;;;   list of datasets
;;; Creates a linear plot from the provided list of datasets. Valid datasets for this plot include line, bar, scatter, and bubble datasets.
;;; @category data, create, plot, plot-category, plot-radial
(define-export plot-linear (js-var "data_plotLinear"))

;;; (plot-category labels & datasets) -> plot?
;;;  labels : any
;;;   list of strings
;;;  datasets : any
;;;   list of datasets
;;; Creates a categorical plot from the provided list of labels and datasets. It is assumed that the dataset provides a value for each label. Valid datasets for this plot include line and bar datasets.
;;; @category data, create, plot, plot-linear, plot-radial
(define-export plot-category (js-var "data_plotCategory"))

;;; (plot-radial labels & datasets) -> plot?
;;;  labels : any
;;;   list of strings
;;;  datasets : any
;;;   list of datasets
;;; Creates a radial plot from the provided list of labels and datasets. It is assumed that the dataset provides a value for each label. Valid datasets for this plot include line and bar datasets.
;;; @category data, create, plot, plot-category, plot-linear
(define-export plot-radial (js-var "data_plotRadial"))

;;; (dataset-line title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers or list of pairs of numbers
;;; Creates a line dataset with the given \`title\` and \`data\` points. If the dataset is used in a numeric (e.g., linear) plot, the data points must be a list of pairs of numbers. If the dataset is used in a categorical plot, the data points must be a list of numbers.
;;; @category data, create, dataset-bar, dataset-bubble, dataset-pie, dataset-polar, dataset-radar, dataset-scatter
(define-export dataset-line (js-var "data_datasetLine"))

;;; (dataset-bar title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers
;;; Creates a bar dataset with the given \`title\` and \`data\` points.
;;; @category data, create, dataset-bubble, dataset-line, dataset-pie, dataset-polar, dataset-radar, dataset-scatter
(define-export dataset-bar (js-var "data_datasetBar"))

;;; (dataset-scatter title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers
;;; Creates a scatter dataset with the given \`title\` and \`data\` points.
;;; @category data, create, dataset-bar, dataset-bubble, dataset-line, dataset-pie, dataset-polar, dataset-radar
(define-export dataset-scatter (js-var "data_datasetScatter"))

;;; (dataset-bubble title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of lists of three numbers
;;; Creates a bubble dataset with the given \`title\` and \`data\` points. The three numbers of each data point represent x-coordinate, y-coordinate, and radius of each point.
;;; @category data, create, dataset-bar, dataset-line, dataset-pie, dataset-polar, dataset-radar, dataset-scatter
(define-export dataset-bubble (js-var "data_datasetBubble"))

;;; (dataset-pie title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers
;;; Creates a pie dataset with the given \`title\` and \`data\` points.
;;; @category data, create, dataset-bar, dataset-bubble, dataset-line, dataset-polar, dataset-radar, dataset-scatter
(define-export dataset-pie (js-var "data_datasetPie"))

;;; (dataset-polar title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers
;;; Creates a polar dataset with the given \`title\` and \`data\` points.
;;; @category data, create, dataset-bar, dataset-bubble, dataset-line, dataset-pie, dataset-radar, dataset-scatter
(define-export dataset-polar (js-var "data_datasetPolar"))

;;; (dataset-radar title data) -> dataset?
;;;  title : string?
;;;  data : any
;;;   list of numbers
;;; Creates a radar dataset with the given \`title\` and \`data\` points.
;;; @category data, create, dataset-bar, dataset-bubble, dataset-line, dataset-pie, dataset-polar, dataset-scatter
(define-export dataset-radar (js-var "data_datasetRadar"))
`],[`file`,`;;; Functions for handling files

;;; (file-exists? fname) -> boolean?
;;;  fname : string?
;;; Returns \`#t\` if \`fname\` names something in storage and \`#f\` otherwise. Note that a directory counts as existing, so a \`#t\` here does not guarantee that \`file->string\` will succeed.
;;; @category file, file->string, file->lines, string->file, lines->file
(define-export file-exists? (js-var "file_fileExistsQ"))

;;; (file->string fname) -> string?
;;;  fname : string?
;;; Reads the contents of the file named \`fname\` and returns it as a string. Raises an error if the file does not exist, so use \`file-exists?\` first if that is a possibility.
;;; @category file, file-exists?, file->lines, string->file, lines->file, with-file
(define-export file->string (js-var "file_fileToString"))

;;; (file->lines fname) -> list?
;;;  fname : string?
;;; Reads the contents of the file named \`fname\` and returns it as a list of strings, one per line. A trailing newline at the end of the file does not produce a final empty line. Raises an error if the file does not exist, so use \`file-exists?\` first if that is a possibility.
;;; @category file, file-exists?, file->string, lines->file, string->file, with-file
(define-export file->lines (js-var "file_fileToLines"))

;;; (string->file str fname) -> void?
;;;  str : string?
;;;  fname : string?
;;; Writes the string \`str\` to the file named \`fname\`, creating the file if it does not exist and replacing its contents if it does.
;;; @category file, lines->file, file->string, file->lines, file-exists?
(define-export string->file (js-var "file_stringToFile"))

;;; (lines->file lines fname) -> void?
;;;  lines : list?
;;;  fname : string?
;;; Writes \`lines\`, a list of strings, to the file named \`fname\`, one line each, creating the file if it does not exist and replacing its contents if it does. A non-empty list is written with a trailing newline, so that \`file->lines\` reads it back unchanged; the empty list writes an empty file. A string that itself contains a newline is written as-is, so it reads back as more than one line.
;;; @category file, string->file, file->lines, file->string, file-exists?
(define-export lines->file (js-var "file_linesToFile"))
`],[`gradescope`,`;;; The Gradescope autograder library

;;
;; A \`gradescope-test-suite-output\` prints as exactly the JSON blob Gradescope
;; reads from \`results/results.json\`, so an autograder is a Scamper program
;; whose last expression is a call to \`gradescope-test-suite\`. The harness that
;; runs one lives in \`gradescope/\` at the root of this repository.
;;
;; N.B., the two struct types are built from the runtime's struct primitives
;; rather than declared with \`struct\`, so that each of their functions carries
;; a docstring (and, from it, a contract) as the rest of the standard library
;; does. \`struct\` is sugar for exactly these calls -- see expansion.ts.

;;; (gradescope-test-result name status score max-score output) -> gradescope-test-result?
;;;  name : string?
;;;   the name Gradescope shows for the case
;;;  status : string?
;;;   "passed" or "failed"
;;;  score : number?
;;;  max-score : number?
;;;  output : any
;;;   the text shown under the case; a string is used as it is, and any other
;;;   value is rendered the way Scamper prints it
;;; Returns a single Gradescope test case. Build one directly for a case that
;;; is not simply one point -- a bonus mark, or work you scored by hand -- and
;;; hand it to \`gradescope-test-suite\` alongside your test results.
;;; @category testing
(define-export gradescope-test-result
  ((js-var "runtime_mkCtorFn")
    "gradescope-test-result"
    ((js-var "prelude_vector") "name" "status" "score" "max-score" "output")))

;;; (gradescope-test-result? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a Gradescope test case.
(define-export gradescope-test-result?
  ((js-var "runtime_mkPredFn") "gradescope-test-result"))

;;; (gradescope-test-result-name c) -> string?
;;;  c : gradescope-test-result?
;;; Returns the name Gradescope shows for the case \`c\`.
;;; @category testing
(define-export gradescope-test-result-name
  ((js-var "runtime_mkGetFn") "gradescope-test-result" "name"))

;;; (gradescope-test-result-status c) -> string?
;;;  c : gradescope-test-result?
;;; Returns the status of the case \`c\`, either "passed" or "failed".
;;; @category testing
(define-export gradescope-test-result-status
  ((js-var "runtime_mkGetFn") "gradescope-test-result" "status"))

;;; (gradescope-test-result-score c) -> number?
;;;  c : gradescope-test-result?
;;; Returns the points the case \`c\` awards.
;;; @category testing
(define-export gradescope-test-result-score
  ((js-var "runtime_mkGetFn") "gradescope-test-result" "score"))

;;; (gradescope-test-result-max-score c) -> number?
;;;  c : gradescope-test-result?
;;; Returns the points the case \`c\` is out of.
;;; @category testing
(define-export gradescope-test-result-max-score
  ((js-var "runtime_mkGetFn") "gradescope-test-result" "max-score"))

;;; (gradescope-test-result-output c) -> any
;;;  c : gradescope-test-result?
;;; Returns the value shown as the output of the case \`c\`.
;;; @category testing
(define-export gradescope-test-result-output
  ((js-var "runtime_mkGetFn") "gradescope-test-result" "output"))

;; N.B., the suite constructor is deliberately *not* exported.
;; \`gradescope-test-suite\` is the only way to build one, so every case a suite
;; holds has been checked to be one -- the renderer
;; (src/js/gradescope/renderers/json.ts) reads their fields without re-checking,
;; and a suite built by hand from the wrong values would otherwise print JSON
;; missing the fields Gradescope needs, which it accepts without complaint.
(define mk-suite-output
  ((js-var "runtime_mkCtorFn")
    "gradescope-test-suite-output"
    ((js-var "prelude_vector") "tests")))

;;; (gradescope-test-suite-output? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is the results of a whole Gradescope test
;;; suite.
(define-export gradescope-test-suite-output?
  ((js-var "runtime_mkPredFn") "gradescope-test-suite-output"))

;;; (gradescope-test-suite-output-tests s) -> list?
;;;  s : gradescope-test-suite-output?
;;; Returns the list of \`gradescope-test-result?\` cases the suite \`s\` holds.
;;; @category testing
(define-export gradescope-test-suite-output-tests
  ((js-var "runtime_mkGetFn") "gradescope-test-suite-output" "tests"))

;; A test result as the Gradescope test case it becomes: worth one point,
;; awarded only when it passed, with the result itself as the case's output so
;; the student reads the same message the IDE would have shown them. A case
;; built by hand is already in its final form and passes straight through.
(define test-result->case
  (lambda (r)
    (match r
      [(gradescope-test-result _ _ _ _ _) r]
      [(test-result-ok desc)
       (gradescope-test-result desc "passed" 1 1 r)]
      [(test-result-error-expected desc _ _)
       (gradescope-test-result desc "failed" 0 1 r)]
      [(test-result-error-exn desc _)
       (gradescope-test-result desc "failed" 0 1 r)]
      [(test-result-error-gen desc _)
       (gradescope-test-result desc "failed" 0 1 r)]
      [_ (error "gradescope-test-suite: expected a list of test results")])))

;;; (gradescope-test-suite tests) -> gradescope-test-suite-output?
;;;  tests : list?
;;;   a list of test-result? values, and any gradescope-test-result? cases
;;;   built by hand
;;; Collects \`tests\` into the results Gradescope expects of an autograder: each
;;; test result is worth one point, awarded only if it passed, and carries its
;;; usual message as its output; a hand-built case is kept as it is. The result
;;; prints as the JSON blob to write to \`results/results.json\`.
;;; @category testing
(define-export gradescope-test-suite
  (lambda (tests)
    (mk-suite-output (map test-result->case tests))))
`],[`html`,`;;; Functions for manipulating HTML DOM elements in Scamper

;;; (element? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an HTML element.
;;; @category html, html?, on-keydown!
(define-export element? (js-var "html_isElement"))

;;; (text-area? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a text area.
;;; @category html, text-area, text-area-get
(define-export text-area? (js-var "html_textAreaQ"))

;;; (text-area id) -> text-area?
;;;  id : string?
;;; Creates a text area with the given id.
;;; @category html, text-area?, text-area-get
(define-export text-area (js-var "html_textArea"))

;;; (text-area-get text-area) -> string?
;;;  text-area : text-area?
;;; Returns the text in the given text area.
;;; @category html, text-area, text-area?
(define-export text-area-get (js-var "html_textAreaGet"))

;;; (button? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a button.
;;; @category html, button
(define-export button? (js-var "html_buttonQ"))

;;; (button label fn) -> button?
;;;  label : string?
;;;  fn : procedure?
;;; Creates a button with the given label and function that is called when the button is pressed.
;;; @category html, button?
(define-export button (js-var "html_button"))

;;; (tag name & c) -> element?
;;;  name : string?
;;;  c : any
;;; Creates an HTML element with the given name and children.
;;; @category html, tag-set-children?
(define-export tag (js-var "html_tag"))

;;; (tag-set-children! elt & c) -> element?
;;;  elt : any
;;;   an HTML element
;;;  c : any
;;;   an HTML element or string
;;; Sets \`elt\`'s children to be \`c1\`, \`c2\`, ..
;;; @category html, mutation, predicates, tag-func
(define-export tag-set-children! (js-var "html_tagSetChildren"))

;;; (on-keydown! fn) -> void?
;;;  fn : procedure?
;;; Calls \`fn\` whenever a key is pressed while the page is focused. \`fn\` takes a single argument, the key pressed by the user as a string.
;;; @category html, mutation, predicates, element?
(define-export on-keydown! (js-var "html_onKeydown"))
`],[`image`,`;;; The Scamper image manipulation library

;;; (canvas? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a canvas.
;;; @category canvas, image, typecheck, predicates, html?
(define-export canvas? (js-var "canvas_canvasQ"))

;;; (html? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an HTML element.
;;; @category html, typecheck, predicates, canvas?
(define-export html? (js-var "html_isElement"))

;;; (color? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a valid color: a string containing a named color, an \`rgb\` value, or an \`hsv\` value.
;;; @category color, hsv, image, predicates, rgb, typecheck, color-func, find-colors, all-color-names
(define-export color? (js-var "color_colorQ"))

;;; (rgb-component? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an integer between 0 and 255.
;;; @category color, image, predicates, rgb, typecheck, rgb-func, color-func, rgb?, rgb-distance
(define-export rgb-component? (js-var "color_isRgbComponent"))

;;; (rgb? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a rgb value.
;;; @category color, image, predicates, rgb, typecheck, rgb-func, color-func, rgb-component?, rgb-distance
(define-export rgb? (js-var "color_isRgb"))

;;; (rgb r g b & a) -> rgb?
;;;  r : rgb-component?
;;;  g : rgb-component?
;;;  b : rgb-component?
;;;  a : rgb-component?
;;;   optional
;;; Returns an rgb value with the specified components.
;;; @category color, image, rgb, color-func, rgb?, rgb-component?, rgb-distance
(define-export rgb (js-var "color_rgb"))

;;; (rgb-red rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the red component of the rgb value.
;;; @category color, image, rgb, rgb-blue, rgb-green
(define-export rgb-red (js-var "color_rgbRed"))

;;; (rgb-green rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the green component of the rgb value.
;;; @category color, image, rgb, rgb-blue, rgb-red
(define-export rgb-green (js-var "color_rgbGreen"))

;;; (rgb-blue rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the blue component of the rgb value.
;;; @category color, image, rgb, rgb-green, rgb-red
(define-export rgb-blue (js-var "color_rgbBlue"))

;;; (rgb-alpha rgb) -> rgb-component?
;;;  rgb : rgb?
;;; Returns the alpha component of the rgb value.
;;; @category color, image, rgb, rgb-hue, rgb-pseudo-complement, rgb-saturation, rgb-value
(define-export rgb-alpha (js-var "color_rgbAlpha"))

;;; (rgb-distance rgb1 rgb2) -> number?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the Euclidean distance between the two rgb values.
;;; @category color, image, rgb, rgb-func, color-func, rgb?, rgb-component?
(define-export rgb-distance (js-var "color_rgbDistance"))

;;; (color-name? v) -> boolean?
;;;  v : string?
;;; Returns \`#t\` if and only if \`v\` is a valid color name.
;;; @category color, image, predicates, typecheck, color-func, all-color-names, find-colors
(define-export color-name? (js-var "color_isColorName"))

;;; (all-color-names) -> list?
;;; Returns a list of all valid color names.
;;; @category color, constants, image, color-func, color?, find-colors, color-name?
(define-export all-color-names (js-var "color_allColorNames"))

;;; (find-colors color-name) -> list?
;;;  color-name : string?
;;; Returns a list of all color names that contain \`color\`, case-insensitive.
;;; @category image, color-func, color?, all-color-names, color-name?
(define-export find-colors (js-var "color_findColors"))

;;; (rgb->string rgb) -> string?
;;;  rgb : rgb?
;;; Returns a string representation of the rgb value, e.g., approrpiate for use as a shape color.
;;; @category color, image, rgb, color-name->rgb, hsv->rgb, rgb->hsv, hsv->string
(define-export rgb->string (js-var "color_rgbToString"))

;;; (hsv? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a hsv value.
;;; @category color, image, hsv, predicates, typecheck, hsv-func
(define-export hsv? (js-var "color_isHsv"))

;;; (hsv h s v & a) -> hsv?
;;;  h : number?
;;;   0 <= h <= 360
;;;  s : number?
;;;   0 <= s <= 100
;;;  v : number?
;;;   0 <= v <= 100
;;;  a : number?
;;;   0 <= a <= 255
;;; Returns a hsv value with the specified components.
;;; @category color, hsv, image, hsv?
(define-export hsv (js-var "color_hsv"))

;;; (hsv-hue hsv) -> number?
;;;  hsv : hsv?
;;; Returns the hue component of the hsv value.
;;; @category color, hsv, image, hsv-alpha, hsv-complement, hsv-saturation, hsv-value
(define-export hsv-hue (js-var "color_hsvHue"))

;;; (hsv-saturation hsv) -> number?
;;;  hsv : hsv?
;;; Returns the saturation component of the hsv value.
;;; @category color, hsv, image, hsv-alpha, hsv-hue, hsv-complement, hsv-value
(define-export hsv-saturation (js-var "color_hsvSaturation"))

;;; (hsv-value hsv) -> number?
;;;  hsv : hsv?
;;; Returns the value component of the hsv value.
;;; @category color, hsv, image, hsv-alpha, hsv-hue, hsv-complement, hsv-saturation
(define-export hsv-value (js-var "color_hsvValue"))

;;; (hsv-alpha hsv) -> number?
;;;  hsv : hsv?
;;; Returns the alpha component of the hsv value.
;;; @category color, hsv, image, hsv-hue, hsv-complement, hsv-saturation, hsv-value
(define-export hsv-alpha (js-var "color_hsvAlpha"))

;;; (hsv-complement hsv) -> hsv?
;;;  hsv : hsv?
;;; Returns the complement of the hsv value.
;;; @category color, hsv, image, hsv-alpha, hsv-hue, hsv-saturation, hsv-value
(define-export hsv-complement (js-var "color_hsvComplement"))

;;; (rgb-hue rgb) -> number?
;;;  rgb : rgb?
;;; Returns the hue component of the rgb value.
;;; @category color, hsv, image, rgb, rgb-alpha, rgb-pseudo-complement, rgb-saturation, rgb-value
(define-export rgb-hue (js-var "color_rgbHue"))

;;; (rgb-saturation rgb) -> number?
;;;  rgb : rgb?
;;; Returns the saturation component of the rgb value.
;;; @category color, hsv, image, rgb, rgb-alpha, rgb-hue, rgb-pseudo-complement, rgb-value
(define-export rgb-saturation (js-var "color_rgbSaturation"))

;;; (rgb-value rgb) -> number?
;;;  rgb : rgb?
;;; Returns the value component of the rgb value.
;;; @category color, hsv, image, rgb, rgb-alpha, rgb-hue, rgb-pseudo-complement, rgb-saturation
(define-export rgb-value (js-var "color_rgbValue"))

;;; (rgb->hsv rgb) -> hsv?
;;;  rgb : rgb?
;;; Converts the rgb value to an hsv value.
;;; @category color, hsv, image, rgb, color-name->rgb, hsv->rgb, rgb->string
(define-export rgb->hsv (js-var "color_rgbToHsv"))

;;; (hsv->string hsv) -> string?
;;;  hsv : hsv?
;;; Returns a string representation of the hsv value.
;;; @category color, hsv, image, rgb->hsv, hcv->rgb
(define-export hsv->string (js-var "color_hsvToString"))

;;; (color-name->rgb color-name) -> rgb?
;;;  color-name : string?
;;; Returns the rgb value of the color name.
;;; @category color, image, rgb, hsv->rgb, rgb->hsv, rgb->string
(define-export color-name->rgb (js-var "color_colorNameToRgb"))

;;; (color->rgb v) -> rgb?
;;;  v : color?
;;; Returns \`v\` as an rgb value, whatever form it was given in.
;;; @category color, image, rgb, color-func, rgb->color-name, describe-color
(define-export color->rgb (js-var "color_colorToRgb"))

;;; (rgb->color-name rgb) -> string?
;;;  rgb : rgb?
;;; Returns the name of the color closest to \`rgb\`. The alpha component is ignored, so only the hue is matched.
;;; @category color, image, rgb, color-func, color->color-name, describe-color
(define-export rgb->color-name (js-var "color_rgbToColorName"))

;;; (color->color-name v) -> string?
;;;  v : color?
;;; Returns the name of the color closest to \`v\`, or \`v\` itself when it already names one.
;;; @category color, image, color-func, rgb->color-name, describe-color
(define-export color->color-name (js-var "color_colorToColorName"))

;;; (describe-color v) -> string?
;;;  v : color?
;;; Returns \`v\` in words, e.g. "red", "semi-transparent blue", or "approximately red" when no name matches exactly. This is how a color reads in the description an image generates for itself.
;;; @category color, image, color-func, color->color-name, describe-image
(define-export describe-color (js-var "color_describeColor"))

;;; (hsv->rgb hsv) -> rgb?
;;;  hsv : hsv?
;;; Converts the hsv value to an rgb value.
;;; @category color, hsv, image, rgb, color-name->rgb, rgb->hsv, rgb->string, hsv->string
(define-export hsv->rgb (js-var "color_hsvToRgb"))

;;; (rgb-darker rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a darker version of the rgb value.
;;; @category color, image, rgb, rgb-lighter
(define-export rgb-darker (js-var "color_rgbDarker"))

;;; (rgb-lighter rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a lighter version of the rgb value.
;;; @category color, image, rgb, rgb-lighter
(define-export rgb-lighter (js-var "color_rgbLighter"))

;;; (rgb-redder rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a redder version of the rgb value.
;;; @category color, image, rgb, rgb-bluer, rgb-greener
(define-export rgb-redder (js-var "color_rgbRedder"))

;;; (rgb-bluer rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a bluer version of the rgb value.
;;; @category color, image, rgb, rgb-greener, rgb-redder
(define-export rgb-bluer (js-var "color_rgbBluer"))

;;; (rgb-greener rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a greener version of the rgb value.
;;; @category color, image, rgb, rgb-bluer, rgb-redder
(define-export rgb-greener (js-var "color_rgbGreener"))

;;; (rgb-pseudo-complement rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a pseudo-complement of the rgb value.
;;; @category color, image, rgb, rgb-greyscale, rgb-phaseshift, rgb-rotate-components
(define-export rgb-pseudo-complement (js-var "color_rgbPseudoComplement"))

;;; (rgb-greyscale rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a greyscale version of the rgb value.
;;; @category color, image, rgb, rgb-phaseshift, rgb-rotate-components
(define-export rgb-greyscale (js-var "color_rgbGreyscale"))

;;; (rgb-phaseshift rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a phaseshifted version of the rgb value.
;;; @category color, image, rgb, rgb-greyscale, rgb-rotate-components
(define-export rgb-phaseshift (js-var "color_rgbPhaseshift"))

;;; (rgb-rotate-components rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a rotated version of the rgb value.
;;; @category color, image, rgb, rgb-greyscale, rgb-phaseshift
(define-export rgb-rotate-components (js-var "color_rgbRotateComponents"))

;;; (rgb-thin rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a thinner version of the rgb value.
;;; @category color, image, rgb, rgb-thicken
(define-export rgb-thin (js-var "color_rgbThin"))

;;; (rgb-thicken rgb) -> rgb?
;;;  rgb : rgb?
;;; Returns a thicker version of the rgb value.
;;; @category color, image, rgb, rgb-thin
(define-export rgb-thicken (js-var "color_rgbThicken"))

;;; (rgb-add rgb1 rgb2) -> rgb?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the sum of the two rgb values.
;;; @category color, image, rgb, rgb-subtract, rgb-average
(define-export rgb-add (js-var "color_rgbAdd"))

;;; (rgb-subtract rgb1 rgb2) -> rgb?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the difference of the two rgb values.
;;; @category color, image, rgb, rgb-add, rgb-average
(define-export rgb-subtract (js-var "color_rgbSubtract"))

;;; (rgb-average rgb1 rgb2) -> rgb?
;;;  rgb1 : rgb?
;;;  rgb2 : rgb?
;;; Returns the average of the two rgb values.
;;; @category color, image, rgb, rgb-add, rgb-subtract
(define-export rgb-average (js-var "color_rgbAverage"))

;;; (font? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a font.
;;; @category image, typecheck, font, text
(define-export font? (js-var "font_fontQ"))

;;; (font face [system-face] [bold?] [italic?]) -> font?
;;;  face : string?
;;;   a valid font name
;;;  system-face : string?
;;;   a generic font family name; defaults to "sans-serif"
;;;  bold? : boolean?
;;;   defaults to #f
;;;  italic? : boolean?
;;;   defaults to #f
;;; Returns a new font value with the given arguments. The \`system-face\` name is drawn from one of the possible system font families, a list can be found on [MDN (font-family)](https://developer.mozilla.org/en-US/docs/Web/CSS/font-family#syntax)
;;; @category image, font?, text
(define-export font (js-var "font_font"))

;;; (drawing? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a drawing: the kind of value the shape constructors build. A canvas is not a drawing, and neither is a loaded image file.
;;; @category image, predicates, typecheck, shapes, canvas?
(define-export drawing? (js-var "drawing_drawingQ"))

;;; (fill-mode? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a fill mode: the string \`"solid"\` or \`"outline"\`.
;;; @category image, shapes, typecheck, predicates, ellipse, rectangle
(define-export fill-mode? (js-var "drawing_fillModeQ"))

;;; (ellipse width height fill color [line-width] [description]) -> drawing?
;;;  width : integer?
;;;  height : integer?
;;;  fill : fill-mode?
;;;  color : color?
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a new drawing containing an ellipse with dimensions \`width × height\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-ellipse, outlined-ellipse
(define-export ellipse (js-var "drawing_ellipse"))

;;; (circle diameter fill color [line-width] [description]) -> drawing?
;;;  diameter : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a circle \`diameter\` wide and \`diameter\` tall, the same size as \`(square diameter fill color)\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-circle, outlined-circle
(define-export circle (js-var "drawing_circle"))

;;; (rectangle width height fill color [line-width] [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a rectangle with dimensions \`width × height\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-rectangle, outlined-rectangle
(define-export rectangle (js-var "drawing_rectangle"))

;;; (square width fill color [line-width] [description]) -> drawing?
;;;  width : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a square with length \`width\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-square, outlined-square
(define-export square (js-var "drawing_square"))

;;; (triangle length fill color [line-width] [description]) -> drawing?
;;;  length : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a equilateral triangle with length \`length\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-triangle, outlined-triangle
(define-export triangle (js-var "drawing_triangle"))

;;; (isosceles-triangle width height fill color [line-width] [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a isosceles triangle with base \`base\` and height \`height\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-isosceles-triangle, outlined-isosceles-triangle
(define-export isosceles-triangle (js-var "drawing_isoscelesTriangle"))

;;; (path width height points fill color [line-width] [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  points : list?
;;;   a list of points, pairs of numbers
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing with dimensions \`width × height\` formed by connecting the points in \`points\` with straight lines. The points are specified as a \`pair\` of coordinates. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, path, with-dash
(define-export path (js-var "drawing_path"))

;;; (beside & d1) -> drawing?
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing \`d1\`, \`d2\`, ..., beside each other (horizontally).
;;; @category composition/placement, image, beside/align, above, above/align, overlay, overlay/align, overlay/offset, rotate
(define-export beside (js-var "drawing_beside"))

;;; (beside/align align & d1) -> drawing?
;;;  align : string?
;;;   either "top", "center", or "bottom"
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing \`d1\`, \`d2\`, ..., beside each other on the x-axis, aligning them along the y-axis according to \`align\`.
;;; @category composition/placement, image, beside, above, above/align, overlay, overlay/align, overlay/offset, rotate
(define-export beside/align (js-var "drawing_besideAlign"))

;;; (above & d1) -> drawing?
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing \`d1\`, \`d2\`, ..., above each other (vertically in descending order).
;;; @category composition/placement, image, beside, beside/align, above/align, overlay, overlay/align, overlay/offset, rotate
(define-export above (js-var "drawing_above"))

;;; (above/align align & d1) -> drawing?
;;;  align : string?
;;;   either "left", "middle", or "right"
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing \`d1\`, \`d2\`, ..., above each other on the y-axis, aligning them along the x-axis according to \`align\`.
;;; @category composition/placement, image, beside, beside/align, above, overlay, overlay/align, overlay/offset, rotate
(define-export above/align (js-var "drawing_aboveAlign"))

;;; (overlay & d1) -> drawing?
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing \`d1\`, \`d2\`, ..., on top of each other. (\`d1\` is the topmost drawing).
;;; @category composition/placement, image, beside, beside/align, above, above/align, overlay/align, overlay/offset, rotate
(define-export overlay (js-var "drawing_overlay"))

;;; (overlay/align xAlign yAlign & d1) -> drawing?
;;;  xAlign : string?
;;;   either "left", "middle", or "right"
;;;  yAlign : string?
;;;   either "top", "center", or "bottom"
;;;  d1 : drawing?
;;; Creates a new drawing formed by places the drawing \`d1\`, \`d2\`, ..., on top of each other, aligning them according to \`xAlign\` and \`yAlign\`.
;;; @category composition/placement, image, beside, beside/align, above, above/align, overlay, overlay/offset, rotate
(define-export overlay/align (js-var "drawing_overlayAlign"))

;;; (overlay/offset dx dy d1 d2) -> drawing?
;;;  dx : number?
;;;  dy : number?
;;;  d1 : drawing?
;;;  d2 : drawing?
;;; Creates a new drawing formed by places the drawing \`d1\` on top of \`d2\`, offset by \`(dx, dy)\`.
;;; @category composition/placement, image, beside, beside/align, above, above/align, overlay, overlay/align, rotate
(define-export overlay/offset (js-var "drawing_overlayOffset"))

;;; (rotate angle d) -> drawing?
;;;  angle : number?
;;;   in degrees
;;;  d : drawing?
;;; Returns a new drawing formed by rotating drawing \`d\` by \`angle\` degrees around the center of its bounding box. Rotating a drawing that is already rotated measures the same as the single equivalent turn, so \`(rotate 30 (rotate 30 d))\` is the size of \`(rotate 60 d)\` rather than larger.
;;; @category image, beside, beside/align, above, above/align, overlay, overlay/align, overlay/offset
(define-export rotate (js-var "drawing_rotate"))

;;; (with-dash dash-spec d) -> drawing?
;;;  dash-spec : list?
;;;   a list of numbers
;;;  d : drawing?
;;; Returns a new drawing formed by drawing \`d\` but with lines drawn according to \`dash-spec\`. \`dash-spec\` is a list of numbers where each successive pair of numbers describe the length of a dash and the length of the subsequent gap.
;;; @category canvas, image, shapes, path-func
(define-export with-dash (js-var "drawing_withDash"))

;;; (text str size color [font]) -> drawing?
;;;  str : string?
;;;  size : number?
;;;   a valid font size (in px)
;;;  color : color?
;;;  font : font?
;;;   defaults to (font "Arial")
;;; Returns a new drawing formed by drawing \`str\` with the given arguments.
;;; @category image, font, font?
(define-export text (js-var "drawing_text"))

;;; (solid-square width color [description]) -> drawing?
;;;  width : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a solid square with length \`width\`.
;;; @category image, shapes, square, outlined-square
(define-export solid-square (js-var "drawing_solidSquare"))

;;; (outlined-square width color [line-width] [description]) -> drawing?
;;;  width : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of an outline square with length \`width\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, square, solid-square
(define-export outlined-square (js-var "drawing_outlinedSquare"))

;;; (solid-rectangle width height color [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a solid rectangle with dimensions \`width × height\`.
;;; @category image, shapes, rectangle, outlined-rectangle
(define-export solid-rectangle (js-var "drawing_solidRectangle"))

;;; (outlined-rectangle width height color [line-width] [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of an outlined rectangle with dimensions \`width × height\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, rectangle, solid-rectangle
(define-export outlined-rectangle (js-var "drawing_outlinedRectangle"))

;;; (solid-circle diameter color [description]) -> drawing?
;;;  diameter : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a solid circle \`diameter\` wide and \`diameter\` tall, the same size as \`(solid-square diameter color)\`.
;;; @category image, shapes, circle, outlined-circle
(define-export solid-circle (js-var "drawing_solidCircle"))

;;; (outlined-circle diameter color [line-width] [description]) -> drawing?
;;;  diameter : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of an outlined circle of diameter \`diameter\`, drawn with a \`line-width\`-wide outline. The drawing is \`diameter\` + \`line-width\` across, since the outline is drawn centred on the circle.
;;; @category image, shapes, circle, solid-circle
(define-export outlined-circle (js-var "drawing_outlinedCircle"))

;;; (solid-ellipse width height color [description]) -> drawing?
;;;  width : integer?
;;;  height : integer?
;;;  color : color?
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a new drawing containing a solid ellipse with dimensions \`width × height\`.
;;; @category image, shapes, ellipse, outlined-ellipse
(define-export solid-ellipse (js-var "drawing_solidEllipse"))

;;; (outlined-ellipse width height color [line-width] [description]) -> drawing?
;;;  width : integer?
;;;  height : integer?
;;;  color : color?
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a new drawing containing an outlined ellipse with dimensions \`width × height\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, ellipse, solid-ellipse
(define-export outlined-ellipse (js-var "drawing_outlinedEllipse"))

;;; (solid-triangle length color [description]) -> drawing?
;;;  length : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a solid equilateral triangle with length \`length\`.
;;; @category image, shapes, triangle, outlined-triangle
(define-export solid-triangle (js-var "drawing_solidTriangle"))

;;; (outlined-triangle length color [line-width] [description]) -> drawing?
;;;  length : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of an outlined equilateral triangle with length \`length\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, triangle, solid-triangle
(define-export outlined-triangle (js-var "drawing_outlinedTriangle"))

;;; (solid-isosceles-triangle width height color [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a solid isosceles triangle with base \`base\` and height \`height\`.
;;; @category image, shapes, isosceles-triangle, outlined-isosceles-triangle
(define-export solid-isosceles-triangle (js-var "drawing_solidIsoscelesTriangle"))

;;; (outlined-isosceles-triangle width height color [line-width] [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of an outlined isosceles triangle with base \`base\` and height \`height\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, isosceles-triangle, solid-isosceles-triangle
(define-export outlined-isosceles-triangle (js-var "drawing_outlinedIsoscelesTriangle"))

;;; (polygon points fill color [line-width] [description]) -> drawing?
;;;  points : (list-of pair?)
;;;   a list of points, pairs of numbers
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing of the polygon whose vertices are \`points\`. Unlike \`path\`, the drawing is sized to fit its own points, so they may be given in whatever coordinates suit. The edges of the polygon should not cross. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-polygon, outlined-polygon, path
(define-export polygon (js-var "drawing_polygon"))

;;; (solid-polygon points color [description]) -> drawing?
;;;  points : (list-of pair?)
;;;   a list of points, pairs of numbers
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing of the solid polygon whose vertices are \`points\`.
;;; @category image, shapes, polygon, outlined-polygon
(define-export solid-polygon (js-var "drawing_solidPolygon"))

;;; (outlined-polygon points color [line-width] [description]) -> drawing?
;;;  points : (list-of pair?)
;;;   a list of points, pairs of numbers
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing of the outlined polygon whose vertices are \`points\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, polygon, solid-polygon
(define-export outlined-polygon (js-var "drawing_outlinedPolygon"))

;;; (diamond width height fill color [line-width] [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a diamond with dimensions \`width × height\`, i.e. the polygon whose vertices are the midpoints of that box's sides. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-diamond, outlined-diamond
(define-export diamond (js-var "drawing_diamond"))

;;; (solid-diamond width height color [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a solid diamond with dimensions \`width × height\`.
;;; @category image, shapes, diamond, outlined-diamond
(define-export solid-diamond (js-var "drawing_solidDiamond"))

;;; (outlined-diamond width height color [line-width] [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of an outlined diamond with dimensions \`width × height\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, diamond, solid-diamond
(define-export outlined-diamond (js-var "drawing_outlinedDiamond"))

;;; (wedge radius angle fill color [line-width] [description]) -> drawing?
;;;  radius : number?
;;;  angle : number?
;;;   how far the wedge sweeps, in degrees
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a wedge: the slice of a circle of radius \`radius\` that sweeps \`angle\` degrees counterclockwise from due east. The size of \`angle\` is what counts, and anything past 360 draws the whole circle. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-wedge, outlined-wedge
(define-export wedge (js-var "drawing_wedge"))

;;; (solid-wedge radius angle color [description]) -> drawing?
;;;  radius : number?
;;;  angle : number?
;;;   how far the wedge sweeps, in degrees
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a solid wedge of radius \`radius\` sweeping \`angle\` degrees.
;;; @category image, shapes, wedge, outlined-wedge
(define-export solid-wedge (js-var "drawing_solidWedge"))

;;; (outlined-wedge radius angle color [line-width] [description]) -> drawing?
;;;  radius : number?
;;;  angle : number?
;;;   how far the wedge sweeps, in degrees
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of an outlined wedge of radius \`radius\` sweeping \`angle\` degrees. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, wedge, solid-wedge
(define-export outlined-wedge (js-var "drawing_outlinedWedge"))

;;; (right-triangle width height fill color [line-width] [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a right triangle with dimensions \`width × height\`, with the right angle at the bottom-left. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, solid-right-triangle, outlined-right-triangle
(define-export right-triangle (js-var "drawing_rightTriangle"))

;;; (solid-right-triangle width height color [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a solid right triangle with dimensions \`width × height\`.
;;; @category image, shapes, right-triangle, outlined-right-triangle
(define-export solid-right-triangle (js-var "drawing_solidRightTriangle"))

;;; (outlined-right-triangle width height color [line-width] [description]) -> drawing?
;;;  width : number?
;;;  height : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of an outlined right triangle with dimensions \`width × height\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, right-triangle, solid-right-triangle
(define-export outlined-right-triangle (js-var "drawing_outlinedRightTriangle"))

;;; (equilateral-triangle edge fill color [line-width] [description]) -> drawing?
;;;  edge : number?
;;;  fill : fill-mode?
;;;   either "solid" or "outline"
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of an equilateral triangle whose sides are \`edge\` long. The same as \`(triangle edge fill color)\`. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, triangle, solid-equilateral-triangle, outlined-equilateral-triangle
(define-export equilateral-triangle (js-var "drawing_equilateralTriangle"))

;;; (solid-equilateral-triangle edge color [description]) -> drawing?
;;;  edge : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of a solid equilateral triangle whose sides are \`edge\` long. The same as \`(solid-triangle edge color)\`.
;;; @category image, shapes, equilateral-triangle, outlined-equilateral-triangle
(define-export solid-equilateral-triangle (js-var "drawing_solidEquilateralTriangle"))

;;; (outlined-equilateral-triangle edge color [line-width] [description]) -> drawing?
;;;  edge : number?
;;;  color : color?
;;;   either a color name or the form "rgba(r, g, b, a)"
;;;  line-width : number?
;;;   how wide the outline is drawn; defaults to 1
;;;  description : string?
;;;   what the image shows, for a screen reader; generated if omitted
;;; Returns a drawing consisting of an outlined equilateral triangle whose sides are \`edge\` long. An outlined shape is \`line-width\` wider and taller than the size it is given, since the outline is drawn centred on that size.
;;; @category image, shapes, equilateral-triangle, solid-equilateral-triangle
(define-export outlined-equilateral-triangle (js-var "drawing_outlinedEquilateralTriangle"))

;;; (ellipse? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an ellipse.
;;; @category image, shapes, predicates, typecheck, circle?, ellipse-width, ellipse-height
(define-export ellipse? (js-var "drawing_ellipseQ"))

;;; (circle? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a circle, i.e. an ellipse that is as wide as it is tall. A circle need not have been made by \`circle\`: \`(circle? (solid-ellipse 20 20 "red"))\` is \`#t\`.
;;; @category image, shapes, predicates, typecheck, ellipse?, circle-diameter
(define-export circle? (js-var "drawing_circleQ"))

;;; (rectangle? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a rectangle.
;;; @category image, shapes, predicates, typecheck, square?, rectangle-width, rectangle-height
(define-export rectangle? (js-var "drawing_rectangleQ"))

;;; (square? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a square, i.e. a rectangle that is as wide as it is tall. A square need not have been made by \`square\`.
;;; @category image, shapes, predicates, typecheck, rectangle?, square-side
(define-export square? (js-var "drawing_squareQ"))

;;; (isosceles-triangle? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an isosceles triangle.
;;; @category image, shapes, predicates, typecheck, equilateral-triangle?, isosceles-triangle-width
(define-export isosceles-triangle? (js-var "drawing_isoscelesTriangleQ"))

;;; (equilateral-triangle? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an equilateral triangle, i.e. an isosceles triangle whose three sides are the same length. It need not have been made by \`equilateral-triangle\`.
;;; @category image, shapes, predicates, typecheck, isosceles-triangle?, equilateral-triangle-edge
(define-export equilateral-triangle? (js-var "drawing_equilateralTriangleQ"))

;;; (polygon? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a polygon. A drawing made by \`path\` is one too, since both are a shape given by its vertices.
;;; @category image, shapes, predicates, typecheck, polygon-points, diamond?, right-triangle?
(define-export polygon? (js-var "drawing_polygonQ"))

;;; (diamond? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a diamond, i.e. a polygon whose four vertices are the midpoints of the sides of its box.
;;; @category image, shapes, predicates, typecheck, polygon?, diamond-width, diamond-height
(define-export diamond? (js-var "drawing_diamondQ"))

;;; (right-triangle? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a right triangle, i.e. a polygon whose three vertices form a right angle at the bottom-left of its box.
;;; @category image, shapes, predicates, typecheck, polygon?, right-triangle-width, right-triangle-height
(define-export right-triangle? (js-var "drawing_rightTriangleQ"))

;;; (wedge? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a wedge.
;;; @category image, shapes, predicates, typecheck, wedge-radius, wedge-angle
(define-export wedge? (js-var "drawing_wedgeQ"))

;;; (solid? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a shape that is filled in. A composition such as \`beside\` is not a shape, so it is neither solid nor outlined.
;;; @category image, shapes, predicates, typecheck, outlined?, fill-mode?
(define-export solid? (js-var "drawing_solidQ"))

;;; (outlined? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a shape that is drawn as an outline. A composition such as \`beside\` is not a shape, so it is neither solid nor outlined.
;;; @category image, shapes, predicates, typecheck, solid?, fill-mode?
(define-export outlined? (js-var "drawing_outlinedQ"))

;;; (ellipse-width e) -> number?
;;;  e : ellipse?
;;; Returns the width \`e\` was given, which for an outlined ellipse is the width inside the outline.
;;; @category image, shapes, ellipse?, ellipse-height
(define-export ellipse-width (js-var "drawing_ellipseWidth"))

;;; (ellipse-height e) -> number?
;;;  e : ellipse?
;;; Returns the height \`e\` was given, which for an outlined ellipse is the height inside the outline.
;;; @category image, shapes, ellipse?, ellipse-width
(define-export ellipse-height (js-var "drawing_ellipseHeight"))

;;; (circle-diameter c) -> number?
;;;  c : circle?
;;; Returns the diameter \`c\` was given, which for an outlined circle is the diameter inside the outline.
;;; @category image, shapes, circle?, ellipse-width
(define-export circle-diameter (js-var "drawing_circleDiameter"))

;;; (rectangle-width r) -> number?
;;;  r : rectangle?
;;; Returns the width \`r\` was given, which for an outlined rectangle is the width inside the outline.
;;; @category image, shapes, rectangle?, rectangle-height
(define-export rectangle-width (js-var "drawing_rectangleWidth"))

;;; (rectangle-height r) -> number?
;;;  r : rectangle?
;;; Returns the height \`r\` was given, which for an outlined rectangle is the height inside the outline.
;;; @category image, shapes, rectangle?, rectangle-width
(define-export rectangle-height (js-var "drawing_rectangleHeight"))

;;; (square-side s) -> number?
;;;  s : square?
;;; Returns the side length \`s\` was given, which for an outlined square is the length inside the outline.
;;; @category image, shapes, square?, rectangle-width
(define-export square-side (js-var "drawing_squareSide"))

;;; (isosceles-triangle-width t) -> number?
;;;  t : isosceles-triangle?
;;; Returns the width \`t\` was given, which for an outlined triangle is the width inside the outline.
;;; @category image, shapes, isosceles-triangle?, isosceles-triangle-height
(define-export isosceles-triangle-width (js-var "drawing_isoscelesTriangleWidth"))

;;; (isosceles-triangle-height t) -> number?
;;;  t : isosceles-triangle?
;;; Returns the height \`t\` was given, which for an outlined triangle is the height inside the outline.
;;; @category image, shapes, isosceles-triangle?, isosceles-triangle-width
(define-export isosceles-triangle-height (js-var "drawing_isoscelesTriangleHeight"))

;;; (equilateral-triangle-edge t) -> number?
;;;  t : equilateral-triangle?
;;; Returns the length of \`t\`'s sides, which for an outlined triangle is the length inside the outline.
;;; @category image, shapes, equilateral-triangle?, isosceles-triangle-width
(define-export equilateral-triangle-edge (js-var "drawing_equilateralTriangleEdge"))

;;; (right-triangle-width t) -> number?
;;;  t : right-triangle?
;;; Returns the width \`t\` was given.
;;; @category image, shapes, right-triangle?, right-triangle-height
(define-export right-triangle-width (js-var "drawing_rightTriangleWidth"))

;;; (right-triangle-height t) -> number?
;;;  t : right-triangle?
;;; Returns the height \`t\` was given.
;;; @category image, shapes, right-triangle?, right-triangle-width
(define-export right-triangle-height (js-var "drawing_rightTriangleHeight"))

;;; (diamond-width d) -> number?
;;;  d : diamond?
;;; Returns the width \`d\` was given.
;;; @category image, shapes, diamond?, diamond-height
(define-export diamond-width (js-var "drawing_diamondWidth"))

;;; (diamond-height d) -> number?
;;;  d : diamond?
;;; Returns the height \`d\` was given.
;;; @category image, shapes, diamond?, diamond-width
(define-export diamond-height (js-var "drawing_diamondHeight"))

;;; (polygon-points p) -> list?
;;;  p : polygon?
;;; Returns the vertices of \`p\` as a list of pairs of numbers, measured from the top-left corner of \`p\` itself rather than in whatever coordinates the polygon was written in.
;;; @category image, shapes, polygon?, polygon
(define-export polygon-points (js-var "drawing_polygonPoints"))

;;; (wedge-radius w) -> number?
;;;  w : wedge?
;;; Returns the radius \`w\` was given.
;;; @category image, shapes, wedge?, wedge-angle
(define-export wedge-radius (js-var "drawing_wedgeRadius"))

;;; (wedge-angle w) -> number?
;;;  w : wedge?
;;; Returns the angle \`w\` sweeps, in degrees.
;;; @category image, shapes, wedge?, wedge-radius
(define-export wedge-angle (js-var "drawing_wedgeAngle"))

;;; (image-description img) -> string?
;;;  img : drawing?
;;; Returns the description of \`img\`: the one it was given, or one built from what it is made of. This is what a screen reader reads out when the image is displayed.
;;; @category image, describe-image, redescribe, describe-color
(define-export image-description (js-var "drawing_drawingDescription"))

;;; (describe-image img) -> string?
;;;  img : drawing?
;;; Returns the description of \`img\`. The same as \`image-description\`.
;;; @category image, image-description, redescribe
(define-export describe-image (js-var "drawing_drawingDescription"))

;;; (redescribe img description) -> drawing?
;;;  img : drawing?
;;;  description : string?
;;; Returns \`img\` with \`description\` as its description. Use this to describe a drawing built with \`beside\`, \`above\`, or \`overlay\`, which take no description of their own.
;;; @category image, image-description, describe-image
(define-export redescribe (js-var "drawing_redescribe"))

;;; (image-width img) -> number?
;;;  img : drawing?
;;; Returns the width of \`img\`. The same as \`drawing-width\`. Note that a shape and an image loaded by \`image-load\` are still separate kinds of value in Scamper, so this takes the former only.
;;; @category image, image-height, drawing-width
(define-export image-width (js-var "drawing_drawingWidth"))

;;; (image-height img) -> number?
;;;  img : drawing?
;;; Returns the height of \`img\`. The same as \`drawing-height\`. Note that a shape and an image loaded by \`image-load\` are still separate kinds of value in Scamper, so this takes the former only.
;;; @category image, image-width, drawing-height
(define-export image-height (js-var "drawing_drawingHeight"))

;;; (image-color img) -> rgb?
;;;  img : drawing?
;;; Returns the color of \`img\`. The same as \`drawing-color\`.
;;; @category image, image-recolor, drawing-color
(define-export image-color (js-var "drawing_drawingColor"))

;;; (image-recolor img color) -> drawing?
;;;  img : drawing?
;;;  color : color?
;;; Returns \`img\` drawn in \`color\`. The same as \`drawing-recolor\`.
;;; @category image, image-color, drawing-recolor
(define-export image-recolor (js-var "drawing_drawingRecolor"))

;;; (drawing-width drawing) -> number?
;;;  drawing : drawing?
;;; Returns the width of the drawing.
;;; @category image, drawing-height
(define-export drawing-width (js-var "drawing_drawingWidth"))

;;; (drawing-height drawing) -> number?
;;;  drawing : drawing?
;;; Returns the height of the drawing.
;;; @category image, drawing-width
(define-export drawing-height (js-var "drawing_drawingHeight"))

;;; (drawing-color drawing) -> rgb?
;;;  drawing : drawing?
;;; Returns the color of the drawing. For a composite drawing, this is the average of its parts' colors.
;;; @category image, drawing-recolor
(define-export drawing-color (js-var "drawing_drawingColor"))

;;; (drawing-recolor drawing color) -> drawing?
;;;  drawing : drawing?
;;;  color : color?
;;; Returns a new drawing with the same dimensions as \`drawing\` but with the color \`color\`.
;;; @category image, drawing-color
(define-export drawing-recolor (js-var "drawing_drawingRecolor"))

;;; (drawing->pixels d) -> vector?
;;;  d : drawing?
;;; Returns a vector of rgb values corresponding to the pixels of the given drawing.
;;; @category image, pixel, drawing->canvas
(define-export drawing->pixels (js-var "drawing_drawingToPixels"))

;;; (drawing->canvas drawing) -> canvas?
;;;  drawing : drawing?
;;; Renders \`drawing\` onto a new canvas and returns it.
;;; @category image, pixel, drawing->pixels
(define-export drawing->canvas (js-var "drawing_drawingToCanvas"))

;;; (with-image-file callback) -> html?
;;;  callback : procedure?
;;; Returns a container with a file chooser that, when used, calls \`callback\` with the selected image and replaces the container's contentsr with the output produced by \`callback\`.
;;; @category image, with-image-from-url, image-load
(define-export with-image-file (js-var "image_withImageFile"))

;;; (image-load fname) -> canvas?
;;;  fname : string?
;;; Reads the image stored in the file named \`fname\` and returns it as a canvas. Raises an error if the file does not exist, if its name does not end in an image extension such as \`.png\` or \`.jpg\`, or if its contents are not a readable image.
;;; @category image, image-save!, with-image-file, with-image-from-url
(define-export image-load (js-var "image_imageLoad"))

;;; (image-save! canvas fname) -> void?
;;;  canvas : canvas?
;;;  fname : string?
;;; Writes \`canvas\` to the file named \`fname\`, creating the file if it does not exist and replacing its contents if it does. The image is written in the format \`fname\` names, which must be one of \`.png\`, \`.jpg\`, \`.jpeg\`, or \`.webp\`.
;;; @category image, image-load, canvas
(define-export image-save! (js-var "image_imageSave"))

;;; (with-image-from-url url callback) -> any
;;;  url : string?
;;;  callback : procedure?
;;; Loads the image at \`url\` and passes it (as a canvas) to \`callback\`. The output of \`callback\` is returned (and rendered to the screen if this is a top-level expression).
;;; @category image, with-image-file, image-load
(define-export with-image-from-url
  (lambda (url callback)
    (callback ((js-var "image_blockOnFetchImage") url))))

;;; (pixel-map fn canvas) -> canvas?
;;;  fn : procedure?
;;;  canvas : canvas?
;;; Returns a new canvas that is the result of applying \`fn\` to each pixel (an rgb value) of \`canvas\`. \`canvas\` itself is unchanged.
;;; @category image, pixel, canvas-get-pixel, canvas->pixels, pixels->canvas, canvas-set-pixels!
(define-export pixel-map
  (lambda (fn canvas)
    (pixels->canvas (vector-map fn (canvas->pixels canvas))
                    (canvas-width canvas)
                    (canvas-height canvas))))

;;; (canvas-get-pixel canvas x y) -> rgb?
;;;  canvas : canvas?
;;;  x : integer?
;;;  y : integer?
;;; Returns the rgb value of the pixel at position \`(x, y)\` of \`canvas\`.
;;; @category color, image, pixel, rgb, pixel-map, canvas->pixels, pixels->canvas, canvas-set-pixels! 
(define-export canvas-get-pixel (js-var "canvas_canvasGetPixel"))

;;; (pixels? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a vector of \`rgb\` values, the representation \`canvas->pixels\` produces and \`pixels->canvas\` consumes.
;;; @category image, pixel, typecheck, predicates, canvas->pixels, pixels->canvas
(define-export pixels? (js-var "canvas_pixelsQ"))

;;; (canvas->pixels canvas) -> pixels?
;;;  canvas : canvas?
;;; Returns the pixels of \`canvas\` as a vector of \`rgb\` values, read left-to-right and top-to-bottom. The result is a snapshot: changing it does not change \`canvas\`. Use \`canvas-set-pixels!\` to write pixels back.
;;; @category image, pixel-map, canvas-get-pixel, pixels->canvas, canvas-set-pixels! 
(define-export canvas->pixels (js-var "canvas_canvasToPixels"))

;;; (pixels->canvas pixels width height) -> canvas?
;;;  pixels : pixels?
;;;  width : integer?
;;;  height : integer?
;;; Returns a new canvas with the given \`pixels\` and dimensions \`width × height\`.
;;; @category image, pixel, pixel-map, canvas-get-pixel, canvas->pixels, canvas-set-pixels! 
(define-export pixels->canvas (js-var "canvas_pixelsToCanvas"))

;;; (canvas-set-pixels! canvas pixels) -> void?
;;;  canvas : canvas?
;;;  pixels : pixels?
;;; Sets the pixels of \`canvas\` to \`pixels\`, mutating it in place.
;;; @category canvas, image, mutation, pixel, predicates, pixel-map, canvas-get-pixel, canvas->pixels, pixels->canvas
(define-export canvas-set-pixels! (js-var "canvas_canvasSetPixels"))

;;; (canvas-width canvas) -> integer?
;;;  canvas : canvas?
;;; Returns the width of the canvas in pixels.
;;; @category canvas, image
(define-export canvas-width (js-var "canvas_canvasWidth"))

;;; (canvas-height canvas) -> integer?
;;;  canvas : canvas?
;;; Returns the height of the canvas in pixels.
;;; @category canvas, image
(define-export canvas-height (js-var "canvas_canvasHeight"))
`],[`lab`,`;;; Functions for creating lab outlines within Scamper source code

;;; (html? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an HTML element.
(define-export html? (js-var "html_isElement"))

;;; (title text) -> html?
;;;  text : string?
;;; Returns a title element.
;;; @category formatting, html?, part, problem, description
(define-export title (js-var "lab_title"))

;;; (part text) -> html?
;;;  text : string?
;;; Returns a part element.
;;; @category formatting, html?, title, problem, description
(define-export part (js-var "lab_part"))

;;; (problem text) -> html?
;;;  text : string?
;;; Returns a problem element.
;;; @category formatting, html?, title, part, description
(define-export problem (js-var "lab_problem"))

;;; (description text) -> html?
;;;  text : string?
;;; Returns a description element.
;;; @category formatting, html?, title, part, problem
(define-export description (js-var "lab_description"))
`],[`music`,`;;; The Scamper music library, inspired from Hudak's Euterpea library for the Haskell programming language

;;; (dur? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a duration.
;;; @category duration, music, predicates, sound, typecheck, dur
(define-export dur? (js-var "music_durQ"))

;;; (dur num den) -> dur?
;;;  num : integer?
;;;  den : integer?
;;; Creates a new duration object representing the ratio \`num/den\`.
;;; @category duration, music, sound, dur?
(define-export dur (js-var "music_dur"))

;;; (numerator dur) -> integer?
;;;  dur : dur?
;;; Returns the numerator of \`dur\`.
;;; @category duration, music, sound, denominator
(define-export numerator (js-var "music_numerator"))

;;; (denominator dur) -> integer?
;;;  dur : dur?
;;; Returns the denominator of \`dur\`.
;;; @category duration, music, sound, numerator
(define-export denominator (js-var "music_denominator"))

;;; (pitch? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a valid pitch, a string denoting a pitch class, e.g., \`"Ab"\`.
;;; @category modifications, music, predicates, sound, typecheck, octave?
(define-export pitch? (js-var "music_isPitchClass"))

;;; (octave? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a valid octave, an integer in the range (0, 10).
;;; @category modifications, music, predicates, sound, typecheck, pitch?
(define-export octave? (js-var "music_isOctave"))

;;; (note-value? n) -> boolean?
;;;  n : number?
;;; Returns \`#t\` if and only \`n\` is a valid MIDI note value (0--127).
;;; @category music, note, predicates, sound, typecheck, note
(define-export note-value? (js-var "music_isValidMidiNote"))

;;; (note midi-note dur) -> composition?
;;;  midi-note : note-value?
;;;  dur : dur?
;;; Creates a new composition consisting of a single note from the given MIDI note value and duration.
;;; @category music, note, sound, note-value?, composition?, empty, instrument, mod, note-event, note-freq, play-composition, repeat, rest, trigger
(define-export note (js-var "music_note"))

;;; (note-freq freq dur) -> composition?
;;;  freq : integer?
;;;   0 <= frequency <= 4000
;;;  dur : dur?
;;; Creates a new composition consisting of a single note of the given frequency and duration.
;;; @category music, note, sound, composition?, empty, instrument, mod, note, note-event, play-composition, repeat, rest, trigger
(define-export note-freq (js-var "music_noteFreq"))

;;; (repeat n comp) -> composition?
;;;  n : integer?
;;;   n >= 0
;;;  comp : composition?
;;; Creates a new composition formed by repeating \`comp\` \`n\` times sequentially.
;;; @category music, sound, composition?, empty, instrument, mod, note, note-event, note-freq, play-composition, rest, trigger
(define-export repeat (js-var "music_repeat"))

;;; (empty) -> composition?
;;; The empty composition.
;;; @category constants, music, note, sound, composition?, instrument, mod, note, note-event, note-freq, play-composition, repeat, rest, trigger
(define-export empty (js-var "music_empty"))

;;; (rest dur) -> composition?
;;;  dur : dur?
;;; Creates a new composition consisting of a single rest from the given duration.
;;; @category music, note, sound, composition?, empty, instrument, mod, note, note-event, note-freq, play-composition, repeat, trigger
(define-export rest (js-var "music_rest"))

;;; (trigger proc) -> composition?
;;;  proc : procedure?
;;;   a procedure that takes no arguments
;;; Creates a new composition that calls the function \`proc\` when played.
;;; @category constants, interactive, music, sound, composition?, empty, instrument, mod, note, note-event, note-freq, play-composition, repeat, rest
(define-export trigger (js-var "music_trigger"))

;;; (par & comp1) -> composition?
;;;  comp1 : composition?
;;; Creates a new composition that plays \`comp1\`, \`comp2\`, ..., in parallel.
;;; @category music, sound, pickup, seq
(define-export par (js-var "music_par"))

;;; (seq & comp1) -> composition?
;;;  comp1 : composition?
;;; Creates a new composition that plays \`comp1\`, \`comp2\`, ..., in sequence.
;;; @category music, sound, par, pickup
(define-export seq (js-var "music_seq"))

;;; (pickup c1 c2) -> composition?
;;;  c1 : composition?
;;;  c2 : composition?
;;; Creates a new composition that plays \`c2\` preceded by \`c1\`. \`c1\`'s duration is not factored into the duration of the overall composition.
;;; @category pickup, par, seq
(define-export pickup (js-var "music_pickup"))

;;; (mod? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a valid modification.
;;; @category modifications, music, predicates, sound, typecheck, dynamics, mod, note-handlers, percussion, tempo
(define-export mod? (js-var "music_modQ"))

;;; percussion: mod?
;;; A modification that switches playback to percussion mode (MIDI channel 9). In percussion mode, each note corresponds to one percussion instrument.
;;; @category constants, modifications, music, sound, load-percussion, dynamics, mod, mod?, note-handlers, tempo
(define-export percussion (js-var "music_percussion"))

;;; (tempo beat bpm) -> mod?
;;;  beat : dur?
;;;   the pulse of the tempo
;;;  bpm : number?
;;;   beats per minute
;;; A modification that plays the modified composition at the given \`beat\` and \`bpm\`.
;;; @category modifications, music, duration, sound, dynamics, mod, mod?, note-handlers, percussion
(define-export tempo (js-var "music_tempo"))

;;; (dynamics velocity) -> mod?
;;;  velocity : integer?
;;;   0 <= level <= 127
;;; A modification that plays the modified composition at the given MIDI \`velocity\` level. Note than a \`velocity\` of \`127\` corresponds to full volume for that note.
;;; @category modifications, music, sound, mod, mod?, note-handlers, percussion, tempo
(define-export dynamics (js-var "music_dynamics"))

;;; (instrument prog) -> composition?
;;;  prog : integer?
;;;   a valid MIDI program number (0--127)
;;; Creates a new composition that plays composition \`comp\` played with MIDI sound or program \`prog\`. See the "General MIDI" Wikipedia article for a complete list of MIDI program numbers to sound mappings. Additionally, you should call \`load-instrument\` at the top-level of your program to download the desired instrument's soundfont before using this function.
;;; @category instruments, music, sound, load-instrument, use-high-quality-instruments, composition?, empty, mod, note, note-event, note-freq, play-composition, repeat, rest, trigger
(define-export instrument (js-var "music_instrument"))

;;; (note-handlers handlers) -> mod?
;;;  handlers : vector?
;;; Create a new modification that plays the composition with the given note handlers. This modification should be used at the top-level of a composition as nested handlers will take precedence over outer handlers.
;;; @category music, note, sound, make-note-handlers, dynamics, mod, mod?, percussion, tempo
(define-export note-handlers (js-var "music_noteHandlers"))

;;; (mod kind comp) -> composition?
;;;  kind : mod?
;;;  comp : composition?
;;; Creates a new composition that plays \`comp\` with the given modification \`mod\`.
;;; @category modifications, music, sound, dynamics, mod?, note-handlers, percussion, tempo, play-composition
(define-export mod (js-var "music_mod"))

;;; (note-event id) -> composition?
;;;  id : any
;;; Creates a new zero-length composition that triggers an event with the given \`id\`.
;;; @category music, note, sound, composition?, empty, instrument, mod, note, note-freq, play-composition, repeat, rest, trigger
(define-export note-event (js-var "music_noteEvent"))

;;; (composition? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a valid composition.
;;; @category music, predicates, sound, typecheck, empty, instrument, mod, note, note-event, note-freq, play-composition, repeat, rest, trigger
(define-export composition? (js-var "music_compositionQ"))

;;; (load-instrument prog) -> void?
;;;  prog : integer?
;;;   a valid MIDI program number (0--127)
;;; Downloads and loads the requested MIDI instrument soundfont.
;;; @category instruments, music, sound, instrument, use-high-quality-instruments
(define-export load-instrument (js-var "music_loadInstrument"))

;;; (load-percussion prog) -> void?
;;;  prog : integer?
;;;   a valid MIDI program number (0--127)
;;; Loads the requested percussion MIDI instrument soundfont.
;;; @category instruments, music, sound, percussion
(define-export load-percussion (js-var "music_loadPercussion"))

;;; (use-high-quality-instruments enable) -> void?
;;;  enable : boolean?
;;;   whether to use high-quality MIDI instruments
;;; Enables (or disables) the use of high-quality MIDI instruments. Note that high-quality instruments are much bigger and take longer to load.
;;; @category instruments, music, sound, instrument, load-instrument
(define-export use-high-quality-instruments (js-var "music_useHighQualityInstruments"))

;;; (make-note-handlers) -> vector?
;;; Makes an empty vector of note handlers appropriate for use with \`note-handler\` and \`on-note\`
;;; @category music, note, sound, note-handlers
(define-export make-note-handlers (js-var "music_makeNoteHandlers"))

;;; (play-composition comp) -> void?
;;;  comp : composition?
;;; Plays the given composition. Note that this function must be triggered from some user action on the screen, _e.g._, a button click. Otherwise, the browser will silently block audio playback.
;;; @category music, sound, composition?, empty, instrument, mod, note, note-event, note-freq, repeat, rest, trigger
(define-export play-composition (js-var "music_playComposition"))

;;; wn: dur?
;;; A whole note duration (4/4).
;;; @category constants, duration, music, sound, hn, qn, en, sn, tn
(define-export wn (js-var "music_wn"))

;;; hn: dur?
;;; A half note duration (2/4).
;;; @category constants, duration, music, sound, wn, qn, en, sn, tn
(define-export hn (js-var "music_hn"))

;;; qn: dur?
;;; A quarter note duration (1/4).
;;; @category constants, music, note, sound, wn, hn, en, sn, tn
(define-export qn (js-var "music_qn"))

;;; en: dur?
;;; An eighth note duration (1/8).
;;; @category constants, duration, music, sound, wn, hn, qn, sn, tn
(define-export en (js-var "music_en"))

;;; sn: dur?
;;; A sixteenth note duration (1/16).
;;; @category constants, duration, music, sound, wn, hn, qn, en, tn
(define-export sn (js-var "music_sn"))

;;; tn: dur?
;;; A thirty-secondth note duration (1/32).
;;; @category duration, music, sound, wn, hn, qn, en, sn
(define-export tn (js-var "music_tn"))
`],[`prelude`,`;;; The Scamper core library

; \`apply\` is a first-class procedure backed by the ap-spread VM primitive
; (native implementation: prelude_apply in src/js/prelude/index.ts).
(define-export apply (js-var "prelude_apply"))

;;; (with-handler handler thunk) -> any?
;;;  handler : procedure?
;;;  thunk : procedure?
;;; Runs \`(thunk)\`. Returns its value if it completes normally; if it raises an
;;; error, calls \`(handler msg)\` with the error's message string and returns that
;;; instead. \`handler\`/\`thunk\` being procedures is enforced by this contract,
;;; which runs before the handler is installed. Native impl: prelude_withHandler.
;;; @category function
(define-export with-handler (js-var "prelude_withHandler"))

; \`error\` raises a runtime error with the given message; a first-class
; procedure backed by prelude_error.
(define-export error (js-var "prelude_error"))

;;; (and☀︎ v1 v2) -> boolean?
;;;  v1 : any
;;;  v2 : any
;;; Returns \`#t\` if and only \`v1\` and \`v2\` are both true.
;;; @category boolean/logic
(define-export and☀︎ (js-var "prelude_equalQ"))

;;; (if☀︎ v1 v2) -> any?
;;;  v1 : any
;;;  v2 : any
;;; Executes \`v2\` if \`v1\` is true
;;; @category boolean/logic
(define-export if☀︎ (js-var "prelude_equalQ"))

;;; (or☀︎ v1 v2) -> boolean?
;;;  v1 : any
;;;  v2 : any
;;; Returns \`#t\` if either \`v1\` or \`v2\` are true.
;;; @category boolean/logic
(define-export or☀︎ (js-var "prelude_equalQ"))

;;; (apply☀︎ v1 v2) -> any?
;;;  v1 : procedure?
;;;  v2 : list?
;;; Applies function \`v1\` to every element of \`v2\`
;;; @category list manipulation
(define-export apply☀︎ (js-var "prelude_equalQ"))

;;; (equal? v1 v2) -> boolean?
;;;  v1 : any
;;;  v2 : any
;;; Returns \`#t\` if and only \`v1\` and \`v2\` are (structurally) equal values.
;;; @category predicates
(define-export equal? (js-var "prelude_equalQ"))

;;; (number? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a number.
;;; @category math, comparator, typecheck, predicates, even?, integer?, negative?, odd?, positive?, real?, zero?
(define-export number? (js-var "prelude_numberQ"))

;;; (real? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a real number.
;;; @category math, comparator, typecheck, predicates, even?, integer?, negative?, number?, odd?, positive?, zero?
(define-export real? (js-var "prelude_realQ"))

;;; (integer? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is an integer.
;;; @category math, comparator, typecheck, predicates, boolean?, char?, string?, even?, negative?, number?, odd?, positive?, real?, zero?
(define-export integer? (js-var "prelude_integerQ"))

;;; (nan? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is the number \`NaN\`.
;;; @category math, comparator, typecheck, predicates, min, max, >=, >, <=, <, = 
(define-export nan? (js-var "prelude_nanQ"))

;;; (< v1 v2) -> boolean?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns \`#t\` if and only \`v1\` is strictly less than \`v2\`.
;;; @category math, comparator, min, max, nan?, >=, >, <=, =
(define-export < (js-var "prelude_lt"))

;;; (<= v1 v2) -> boolean?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns \`#t\` if and only \`v1\` is less than or equal to \`v2\`.
;;; @category math, comparator, min, max, nan?, >=, >, <, =
(define-export <= (js-var "prelude_leq"))

;;; (> v1 v2) -> boolean?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns \`#t\` if and only \`v1\` is strictly greater than \`v2\`.
;;; @category math, comparator, min, max, nan?, >=, <=, <, =
(define-export > (js-var "prelude_gt"))

;;; (>= v1 v2) -> boolean?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns \`#t\` if and only \`v1\` is greater than or equal to \`v2\`.
;;; @category math, comparator, min, max, nan?, >, <=, <, =
(define-export >= (js-var "prelude_geq"))

;;; (= v1 v2) -> boolean?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns \`#t\` if and only \`v1\` is equal to \`v2\`.
;;; @category math, comparator, min, max, nan?, >=, >, <=, <
(define-export = (js-var "prelude_eq"))

;;; (=-eps n) -> procedure?
;;;  n : number?
;;; Returns a function that takes two numbers \`x\` and \`y\` as input returns \`#t\` if \`|x - y| < n\`.
;;; @category function composition, all-of, any-of, compose, o, |>
(define-export =-eps (js-var "prelude_equalsEps"))

;;; (zero? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is zero.
;;; @category comparator, math, predicates, typecheck, even?, integer?, negative?, number?, odd?, positive?, real?
(define-export zero? (js-var "prelude_zeroQ"))

;;; (positive? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is positive.
;;; @category math, comparator, typecheck, predicates, even?, integer?, negative?, number?, odd?, real?, zero?
(define-export positive? (js-var "prelude_positiveQ"))

;;; (negative? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is negative.
;;; @category math, comparator, typecheck, predicates, even?, integer?, number?, odd?, positive?, real?, zero?
(define-export negative? (js-var "prelude_negativeQ"))

;;; (odd? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is odd.
;;; @category math, comparator, typecheck, predicates, even?, integer?, negative?, number?, positive?, real?, zero?
(define-export odd? (js-var "prelude_oddQ"))

;;; (even? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is even.
;;; @category math, comparator, predicates, integer?, negative?, number?, odd?, positive?, real?, zero?
(define-export even? (js-var "prelude_evenQ"))

;;; (max & v) -> number?
;;;  v : number?
;;; Returns the maximum of the given numbers.
;;; @category math, comparator, min, nan?, >=, >, <=, <, =
(define-export max (js-var "prelude_max"))

;;; (min & v) -> number?
;;;  v : number?
;;; Returns the minimum of the given numbers.
;;; @category math, comparator, max, nan?, >=, >, <=, <, =
(define-export min (js-var "prelude_min"))

;;; (+ & v1) -> number?
;;;  v1 : number?
;;; Returns the sum of \`v1\`, \`v2\`, ... .
;;; @category math, algebra, -, *, /, modulo, quotient, remainder 
(define-export + (js-var "prelude_plus"))

;;; (- v1 & v2) -> number?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns the difference of \`v1\`, \`v2\`, ... .
;;; @category math, algebra, +, *, /, modulo, quotient, remainder
(define-export - (js-var "prelude_minus"))

;;; (* & v1) -> number?
;;;  v1 : number?
;;; Returns the product of \`v1\`, \`v2\`, ... .
;;; @category math, algebra, +, -, /, modulo, quotient, remainder
(define-export * (js-var "prelude_times"))

;;; (/ v1 & v2) -> number?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns the quotient of \`v1\`, \`v2\`, ... .
;;; @category math, algebra, +, -, *, modulo, quotient, remainder
(define-export / (js-var "prelude_div"))

;;; (abs v) -> number?
;;;  v : number?
;;; Returns the absolute value of \`v\`.
;;; @category math, algebra, ceiling, floor, round, truncate
(define-export abs (js-var "prelude_abs"))

;;; (quotient v1 v2) -> number?
;;;  v1 : integer?
;;;  v2 : integer?
;;; Returns the quotient of \`v1\` and \`v2\`, _i.e._, the whole number part of \`v1 / v2\`.
;;; @category math, algebra, +, -, *, /, modulo, remainder
(define-export quotient (js-var "prelude_quotient"))

;;; (remainder v1 v2) -> number?
;;;  v1 : integer?
;;;  v2 : integer?
;;; Returns the remainder of \`v1\` and \`v2\`, _i.e._, the remainder of \`v1 / v2\`.
;;; @category math, algebra, +, -, *, /, modulo, quotient
(define-export remainder (js-var "prelude_remainder"))

;;; (modulo v1 v2) -> number?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns \`k = n - d * q\` where \`q\` is the integer such that \`k\` has the same sign as the divisor \`d\` while being as close to 0 as possible. (Source: [MDN docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Remainder).)
;;; @category math, algebra, +, -, *, /, quotient, remainder
(define-export modulo (js-var "prelude_modulo"))

;;; (floor v) -> integer?
;;;  v : number?
;;; Returns the largest integer less than or equal to \`v\`.
;;; @category math, algebra, abs, ceiling, round, truncate
(define-export floor (js-var "prelude_floor"))

;;; (ceiling v) -> integer?
;;;  v : number?
;;; Returns the smallest integer greater than or equal to \`v\`.
;;; @category math, algebra, abs, floor, round, truncate
(define-export ceiling (js-var "prelude_ceiling"))

;;; (truncate v) -> integer?
;;;  v : number?
;;; Returns the integer closest to \`v\` less than or equal to \`v\`.
;;; @category math, algebra, abs, ceiling, floor, round
(define-export truncate (js-var "prelude_truncate"))

;;; (round v) -> integer?
;;;  v : number?
;;; Returns the integer closest to \`v\`.
;;; @category math, algebra, abs, ceiling, floor, truncate
(define-export round (js-var "prelude_round"))

;;; (square v) -> number?
;;;  v : number?
;;; Returns the square of \`v\`.
;;; @category math, algebra, exp, expt, sqrt, log
(define-export square (js-var "prelude_square"))

;;; (sqrt v) -> number?
;;;  v : number?
;;; Returns the square root of \`v\`.
;;; @category math, algebra, exp, expt, square, log
(define-export sqrt (js-var "prelude_sqrt"))

;;; (expt x y) -> number?
;;;  x : number?
;;;  y : number?
;;; Returns \`x\` raised to the power of \`y\`.
;;; @category math, algebra, exp, square, sqrt, log  
(define-export expt (js-var "prelude_expt"))

;;; (number->string v) -> string?
;;;  v : number?
;;; Returns the string representation of \`v\`.
;;; @category string, char->integer, digit-value, integer->char, string->number
(define-export number->string (js-var "prelude_numberToString"))

;;; (string->number s) -> number?
;;;  s : string?
;;; Returns the number denoted by \`s\`, or \`#f\` if \`s\`
;;; does not denote a number.
;;; @category string, number->string, string->list, string->words, string->vector, char->integer, digit-value, integer->char
(define-export string->number (js-var "prelude_stringToNumber"))

;;; (exp v) -> number?
;;;  v : number?
;;; Returns the exponential of \`v\`.
;;; @category math, algebra, expt, square, sqrt, log
(define-export exp (js-var "prelude_exp"))

;;; (log v) -> number?
;;;  v : number?
;;; Returns the natural logarithm of \`v\`.
;;; @category math, algebra, exp, expt, square, sqrt
(define-export log (js-var "prelude_log"))

;;; (sin v) -> number?
;;;  v : number?
;;; Returns the sine of \`v\`.
;;; @category math, trigonometry, acos, asin, atan, cos, tan, pi, π  
(define-export sin (js-var "prelude_sin"))

;;; (cos v) -> number?
;;;  v : number?
;;; Returns the cosine of \`v\`.
;;; @category math, trigonometry, acos, asin, atan, sin, tan, pi, π  
(define-export cos (js-var "prelude_cos"))

;;; (tan v) -> number?
;;;  v : number?
;;; Returns the tangent of \`v\`.
;;; @category math, trigonometry, acos, asin, atan, cos, sin, pi, π  
(define-export tan (js-var "prelude_tan"))

;;; (asin v) -> number?
;;;  v : number?
;;; Returns the arc sine of \`v\`.
;;; @category math, trigonometry, acos, atan, cos, sin, tan, pi, π  
(define-export asin (js-var "prelude_asin"))

;;; (acos v) -> number?
;;;  v : number?
;;; Returns the arc cosine of \`v\`.
;;; @category math, trigonometry, asin, atan, cos, sin, tan, pi, π  
(define-export acos (js-var "prelude_acos"))

;;; (atan v) -> number?
;;;  v : number?
;;; Returns the arc tangent of \`v\`.
;;; @category math, trigonometry, acos, asin, cos, sin, tan, pi, π  
(define-export atan (js-var "prelude_atan"))

;;; (not v) -> boolean?
;;;  v : boolean?
;;; Returns \`#t\` if and only \`v\` is \`#f\`.
;;; @category boolean/logic, and, nand, nor, xor, or
(define-export not (js-var "prelude_not"))

;;; (boolean? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a boolean.
;;; @category typecheck, boolean/logic, predicates, char?, string?, integer?
(define-export boolean? (js-var "prelude_booleanQ"))

;;; (nand & v1) -> boolean?
;;;  v1 : boolean?
;;; Equivalent to \`(not (and v1 v2 ...))\`.
;;; @category boolean/logic, and, nor, not, xor, or
(define-export nand (js-var "prelude_nand"))

;;; (nor & v1) -> boolean?
;;;  v1 : boolean?
;;; Equivalent to \`(not (or v1 v2 ...))\`.
;;; @category boolean/logic, and, nand, not, xor, or
(define-export nor (js-var "prelude_nor"))

;;; (implies v1 v2) -> boolean?
;;;  v1 : boolean?
;;;  v2 : boolean?
;;; Equivalent to \`(if v1 v2 #t)\`.
;;; @category boolean/logic, predicates
(define-export implies (js-var "prelude_implies"))

;;; (xor v1 v2) -> boolean?
;;;  v1 : boolean?
;;;  v2 : boolean?
;;; Equivalent to \`(or (and v1 (not v2)) (and (not v1) v2))\`.
;;; @category boolean/logic, and, nand, nor, not, or
(define-export xor (js-var "prelude_xor"))

;;; (any-of & f1) -> procedure?
;;;  f1 : any
;;;   procedure? that takes a value as input and returns a boolean.
;;; Returns a unary function that returns \`#t\` if and only one of \`f1\`, \`f2\`, ... is \`#t\` for its argument.
;;; @category function composition, boolean/logic, all-of, compose, =-eps, o, |>
(define-export any-of
  (lambda (& fs)
    (lambda (v)
      (some-satisfy? (lambda (g) (g v)) fs))))

;;; (all-of & f1) -> procedure?
;;;  f1 : any
;;;   procedure? that takes a value as input and returns a boolean.
;;; Returns a unary function that returns \`#t\` if and only all of \`f1\`, \`f2\`, ... are \`#t\` for its argument.
;;; @category function composition, boolean/logic, any-of, compose, =-eps, o, |>
(define-export all-of
  (lambda (& fs)
    (lambda (v)
      (all-satisfy? (lambda (g) (g v)) fs))))

;;; (pair? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a pair.
;;; @category typecheck, predicates, list?, null?, procedure?, ref?, rex?, vector?, void?
(define-export pair? (js-var "prelude_pairQ"))

;;; (list-of p) -> procedure?
;;;  p : procedure?
;;;   returns \`#t\` if its argument is of the desired type
;;; Returns a new predicate that tests whether its argument is a list of elements that satisfy the predicate \`p\`.
;;; @category list, function composition, association list, apply, filter, fold, fold-left, fold-right, for-range, map, reduce, reduce-right
(define-export list-of
  (lambda (p)
    (lambda (l)
      (and (list? l) (all-satisfy? p l)))))

;; N.B., deliberately plain Scamper (not js-var-backed): the contract-check
;; codegen (contract.ts) needs to call an arbitrary predicate -- possibly a
;; user-defined closure -- once per element of a variadic argument's
;; collected rest-list, and JS code can no longer call back into Scamper
;; (Closure.call/callScamperFn are both disabled). Written in Scamper, the
;; call to pred? is ordinary application, so it works uniformly whether
;; pred? is a closure or a js-var-backed primitive.
;; N.B., left undocumented like "any", for the same reason: it's
;; compiler-support infrastructure, not a documented user-facing binding.
(define-export all-satisfy?
  (lambda (pred? lst)
    (if (null? lst)
        #t
        (if (pred? (car lst))
            (all-satisfy? pred? (cdr lst))
            #f))))

;;; (cons v1 v2) -> pair?
;;;  v1 : any
;;;  v2 : any
;;; Returns a new cons cell containing \`v1\` and \`v2\`.
;;; @category list, list manipulation, association list, car, cdr
(define-export cons (js-var "prelude_cons"))

;;; (pair v1 v2) -> pair?
;;;  v1 : any
;;;  v2 : any
;;; Returns a new pair containing \`v1\` and \`v2\`.
;;; @category list, list creation, list, string, regex, vector, void
(define-export pair (js-var "prelude_pair"))

;; N.B., left undocumented, like all-satisfy?: it's contract-support
;; infrastructure (or/p's fold over its predicates), not a user-facing
;; binding. Documenting it would make contract.ts wrap it in a checking
;; lambda whose own checks call all-satisfy? -> car/cdr -> back into the
;; contracted car/cdr, so it MUST stay undocumented to avoid that cycle.
;; Uses match (not car/cdr) to walk the list for the same reason: match
;; decomposes via the runtime's pMatch, so it never reaches contracted car/cdr.
(define-export some-satisfy?
  (lambda (pred? lst)
    (match lst
      [null #f]
      [(cons x rest) (if (pred? x) #t (some-satisfy? pred? rest))])))

;; N.B., or/p combines predicates disjunctively: (or/p p q) is a predicate
;; that holds when p OR q holds. Written in plain Scamper (not a js-var) so
;; that applying each predicate is ordinary Scamper application -- JS code can
;; no longer call back into Scamper (Closure.call/callScamperFn are disabled).
;; Left UNDOCUMENTED for the same reason as some-satisfy?: a docstring would
;; make contract.ts wrap it, and that wrapper's checks would recurse through
;; car/cdr into or/p (which car/cdr's own contract references), re-creating the
;; cycle. Requires at least one predicate: the arglist grammar has no
;; zero-fixed-param rest form, and or/p over no predicates is never needed.
(define-export or/p
  (lambda (first & rest)
    (lambda (x) (some-satisfy? (lambda (p) (p x)) (cons first rest)))))

;;; (car v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Returns the first element of \`v\`.
;;; @category list, list manipulation, association list, cdr, cons
(define-export car (js-var "prelude_car"))

;;; (cdr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Returns the second element of \`v\`.
;;; @category list, list manipulation, association list, car, cons
(define-export cdr (js-var "prelude_cdr"))

;;; (null? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is the empty list.
;;; @category list, association list, typecheck, predicates, pair?, list?, procedure?, ref?, vector?, void?
(define-export null? (js-var "prelude_nullQ"))

;;; (list? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a list.
;;; @category list, association list, typecheck, predicates, pair?, null?, procedure?, ref?, rex?, vector?, void?
(define-export list? (js-var "prelude_listQ"))

;;; (nonempty-list? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a non-empty list.
;;; @category typecheck, predicates
(define-export nonempty-list? (js-var "prelude_nonemptyListQ"))

;;; (list & v1) -> list?
;;;  v1 : any
;;; Returns a new list containing \`v1\`, \`v2\`, ... .
;;; @category list, list creation, association list, pair, string, regex, vector, void
(define-export list (js-var "prelude_list"))

;;; (make-list n v) -> list?
;;;  n : integer?
;;;  v : any
;;; Returns a new list containing \`n\` copies of \`v\`.
;;; @category list, list creation, association list, make-string, make-vector, append, list-drop, list-tail, list-take, range, reverse, sort
(define-export make-list (js-var "prelude_makeList"))

;;; (length v) -> integer?
;;;  v : list?
;;; Returns the length of \`v\`.
;;; @category list, list manipulation, index-of, range, string-length, vector-length, vector-range, vector-ref 
(define-export length (js-var "prelude_length"))

;;; (append & l1) -> list?
;;;  l1 : list?
;;; Returns a new list containing the elements of lists \`l1\`, \`l2\`, ... in sequence.
;;; @category list, list manipulation, list-drop, list-tail, list-take, make-list, range, reverse, sort
(define-export append (js-var "prelude_append"))

;;; (reverse l) -> list?
;;;  l : list?
;;; Returns a new list containing the elements of \`l\` in reverse order.
;;; @category list, list manipulation, append, list-drop, list-tail, list-take, make-list, make-string, range, sort
(define-export reverse (js-var "prelude_reverse"))

;;; (list-tail l k) -> list?
;;;  l : list?
;;;  k : integer?
;;;   0 <= k <= (length l)
;;; Returns \`l\` but with the first \`k\` elements of \`l\` omitted.
;;; @category list, list manipulation, association list, append, list-drop, list-take, make-list, range, reverse, sort
(define-export list-tail (js-var "prelude_listTail"))

;;; (list-take l k) -> list?
;;;  l : list?
;;;  k : integer?
;;;   0 <= k <= (length l)
;;; Returns a new list containing the first \`k\` elements of \`l\`.
;;; @category list, list manipulation, association list, append, list-drop, list-tail, make-list, range, reverse, sort
(define-export list-take (js-var "prelude_listTake"))

;;; (list-drop l k) -> list?
;;;  l : list?
;;;  k : integer?
;;;   0 <= k <= (length l)
;;; An alias for \`(list-tail l k)\`.
;;; @category list, list manipulation, association list, append, list-tail, list-take, make-list, range, reverse, sort
(define-export list-drop (js-var "prelude_listDrop"))

;;; (list-ref l n) -> any
;;;  l : list?
;;;  n : integer?
;;;   0 <= n < (length l)
;;; Returns the \`n\`th element of \`l\`.
;;; @category list, association list, assoc-ref, deref, ref, ref-set!, string-ref
(define-export list-ref (js-var "prelude_listRef"))

;;; (index-of v l) -> integer?
;;;  v : any
;;;  l : list?
;;; Returns the index of the first occurrence of \`v\` in \`l\` or \`-1\` if \`v\` is not in \`l\`.
;;; @category list, list manipulation, association list, range, string-length, vector-length, vector-range, vector-ref 
(define-export index-of (js-var "prelude_indexOf"))

;;; (assoc-key? k l) -> any
;;;  k : any
;;;  l : list?
;;;   an association list
;;; Returns \`#t\` if \`k\` is a key in association list \`l\`.
;;; @category list, list manipulation, association list, predicates, assoc-ref, assoc-set
(define-export assoc-key? (js-var "prelude_assocKey"))

;;; (assoc-ref k l) -> any
;;;  k : any
;;;  l : list?
;;;   an association list
;;; Returns the value associated with key \`k\` in association list \`l\`.
;;; @category list, list manipulation, association list, deref, list-ref, ref, ref-set!, string-ref, assoc-key?
(define-export assoc-ref (js-var "prelude_assocRef"))

;;; (assoc-set k v l) -> list?
;;;  k : any
;;;  v : any
;;;  l : list?
;;;   an association list
;;; Returns a new association list containing the same key-value pairs as \`l\` except that \`k\` is associated with \`v\`.
;;; @category list, list manipulation, association list, assoc-key?, assoc-ref
(define-export assoc-set (js-var "prelude_assocSet"))

;; N.B., internal helper for sort: merges two lists that are each already
;; sorted by lt? into one sorted list. Undocumented (like all-satisfy?) so the
;; contract-check codegen leaves it alone. Ties favor xs (the left list) to
;; keep sort stable.
(define-export sort-merge
  (lambda (lt? xs ys)
    (cond
      [(null? xs) ys]
      [(null? ys) xs]
      [(lt? (car ys) (car xs)) (cons (car ys) (sort-merge lt? xs (cdr ys)))]
      [else (cons (car xs) (sort-merge lt? (cdr xs) ys))])))

;;; (sort l lt?) -> list?
;;;  l : list?
;;;  lt? : procedure?
;;;   returns \`#t\` if the first arg is less than the second
;;; Returns a new list containing the elements of \`l\` sorted in ascending order according to the comparison function \`lt?\`.
;;; @category list, list manipulation, association list, append, list-drop, list-tail, list-take, make-list, make-string, range, reverse
(define-export sort
  (lambda (l lt?)
    (if (<= (length l) 1)
        l
        (let ([mid (quotient (length l) 2)])
          (sort-merge lt?
                      (sort (list-take l mid) lt?)
                      (sort (list-drop l mid) lt?))))))

;;; (char? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a character.
;;; @category typecheck, char, predicates, boolean?, string?, integer?
(define-export char? (js-var "prelude_charQ"))

;;; (digit-value c) -> integer?
;;;  c : char?
;;; Returns the numeric value of \`c\` if \`c\` is a decimal digit (0-10), otherwise raises an error.
;;; @category typecheck, char, char->integer, integer->char, number->string, string->number
(define-export digit-value (js-var "prelude_digitalValue"))

;;; (char->integer c) -> integer?
;;;  c : char?
;;; Returns the codepoint value of character \`c\`.
;;; @category char, digit-value, integer->char, number->string, string->number
(define-export char->integer (js-var "prelude_charToInteger"))

;;; (integer->char n) -> char?
;;;  n : integer?
;;; Returns the character with codepoint value \`n\`.
;;; @category char, char->integer, digit-value, number->string, string->number
(define-export integer->char (js-var "prelude_integerToChar"))

;;; (char-upcase c) -> char?
;;;  c : char?
;;; Returns the upper-case equivalent of \`c\`.
;;; @category char, char-downcase, char-foldcase
(define-export char-upcase (js-var "prelude_charUpcase"))

;;; (char-downcase c) -> char?
;;;  c : char?
;;; Returns the lower-case equivalent of \`c\`.
;;; @category char, char-upcase, char-foldcase
(define-export char-downcase (js-var "prelude_charDowncase"))

;;; (char-foldcase c) -> char?
;;;  c : char?
;;; Returns the case-folded equivalent of \`c\`. This is a version of \`c\` that is appropriate for case-insensitive comparison.
;;; @category char, char-upcase, char-downcase
(define-export char-foldcase (js-var "prelude_charFoldcase"))

;;; (string? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a string.
;;; @category typecheck, string, predicates, boolean?, char?, integer? 
(define-export string? (js-var "prelude_stringQ"))

;;; (make-string k c) -> string?
;;;  k : integer?
;;;  c : char?
;;; Returns a string of length \`k\` with each character set to \`c\`.
;;; @category string, make-list, make-vector, string-append, string-map
(define-export make-string (js-var "prelude_makeString"))

;;; (string & c1) -> string?
;;;  c1 : char?
;;; Returns a string consisting of the characters \`c1\`, \`c2\`, ...
;;; @category string, list, pair, rex, vector
(define-export string (js-var "prelude_string"))

;;; (string-length v) -> integer?
;;;  v : string?
;;; Returns the length of \`v\`.
;;; @category string, index-of, length, range, string-length, vector-range, vector-ref 
(define-export string-length (js-var "prelude_stringLength"))

;;; (string-ref s n) -> char?
;;;  s : string?
;;;  n : integer?
;;; Returns the character at index \`n\` of string \`s\`.
;;; @category string, assoc-ref, deref, list-ref, ref, ref-set!
(define-export string-ref (js-var "prelude_stringRef"))

;;; (string-upcase s) -> string?
;;;  s : string?
;;; Returns the upper-case version of \`s\`.
;;; @category string, string-downcase, string-foldcase, substring, string-split, string-split-vector
(define-export string-upcase (js-var "prelude_stringUpcase"))

;;; (string-downcase s) -> string?
;;;  s : string?
;;; Returns the lower-case version of \`s\`.
;;; @category string, string-upcase, string-foldcase, substring, string-split, string-split-vector
(define-export string-downcase (js-var "prelude_stringDowncase"))

;;; (string-foldcase s) -> string?
;;;  s : string?
;;; Returns the case-folded version of \`s\`. This is a version of \`s\` that is appropriate for case-insensitive comparison.
;;; @category string, string-downcase, string-upcase, substring, string-split, string-split-vector
(define-export string-foldcase (js-var "prelude_stringFoldcase"))

;;; (substring s start [end]) -> string?
;;;  s : string?
;;;  start : integer?
;;;  end : integer?
;;;   where the substring ends; defaults to the end of \`s\`
;;; Returns the substring of \`s\` from index \`start\` (inclusive) to index \`end\` (exclusive), or to the end of \`s\` when \`end\` is left out.
;;; @category string, string-downcase, string-upcase, string-foldcase, string-split, string-split-vector
(define-export substring (js-var "prelude_substring"))

;;; (string-append & s1) -> string?
;;;  s1 : string?
;;; Returns a string made by joining \`s1\`, \`s2\`, ... together.
;;; @category string, append, make-string, range, string-map
(define-export string-append (js-var "prelude_stringAppend"))

;;; (string->list s) -> list?
;;;  s : string?
;;; Returns a list of the characters in \`s\`.
;;; @category list, list creation, string, list->string, string->number, string->words, string->vector
(define-export string->list (js-var "prelude_stringToList"))

;;; (list->string l) -> string?
;;;  l : list?
;;; Returns a string made by joining the characters in \`l\` together.
;;; @category list, list manipulation, association list, string->list, list->vector
(define-export list->string (js-var "prelude_listToString"))

;;; (string->vector s) -> vector?
;;;  s : string?
;;; Returns a vector of the characters in \`s\`.
;;; @category string, vectors, vector->string, string->list, string->number, string->words
(define-export string->vector (js-var "prelude_stringToVector"))

;;; (vector->string v) -> string?
;;;  v : vector?
;;; Returns a string made by joining the characters in \`v\` together.
;;; @category vectors, string->vector, vector->list
(define-export vector->string (js-var "prelude_vectorToString"))

;;; (string-contains s1 s2) -> boolean?
;;;  s1 : string?
;;;  s2 : string?
;;; Returns \`#t\` if and only if string \`s1\` contains string \`s2\`.
;;; @category string, string=?, string>=?, string>?, string<=?, string<?, string-ci=?, string-ci>=?, string-ci>?, string-ci<=?, string-ci<?
(define-export string-contains (js-var "prelude_stringContains"))

;;; (string-split s sep) -> list?
;;;  s : string?
;;;  sep : string?
;;; Returns a list of strings obtained by splitting \`s\` at occurrences of \`sep\`.
;;; One \`sep\` at each end of \`s\` is ignored, so it does not produce an empty
;;; string in the result; a string that is nothing but \`sep\` splits into
;;; nothing at all.
;;; @category string, string-downcase, string-upcase, string-foldcase, substring, string-split-vector
(define-export string-split (js-var "prelude_stringSplit"))

;;; (string-split-vector s sep) -> vector?
;;;  s : string?
;;;  sep : string?
;;; Returns a vector of strings obtained by splitting \`s\` at occurrences of \`sep\`.
;;; One \`sep\` at each end of \`s\` is ignored, so it does not produce an empty
;;; string in the result; a string that is nothing but \`sep\` splits into
;;; nothing at all.
;;; @category string, vectors, string-downcase, string-upcase, string-foldcase, substring, string-split 
(define-export string-split-vector (js-var "prelude_stringSplitVector"))

;;; (vector? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a vector.
;;; @category typecheck, vectors, predicates, pair?, list?, null?, procedure?, ref?, rex?, void?
(define-export vector? (js-var "prelude_vectorQ"))

;;; (vector & v1) -> vector?
;;;  v1 : any
;;; Returns a vector consisting of the values \`v1\`, \`v2\`, ...
;;; @category vectors, list, pair, string, regex, void
(define-export vector (js-var "prelude_vector"))

;;; (make-vector k v) -> vector?
;;;  k : integer?
;;;  v : any
;;; Returns a vector of length \`k\` with each element set to \`v\`.
;;; @category vectors, make-list, make-string
(define-export make-vector (js-var "prelude_makeVector"))

;;; (vector-length v) -> integer?
;;;  v : vector?
;;; Returns the length of vector \`v\`.
;;; @category vectors, vector-range, vector-ref, string-length, index-of, length, range
(define-export vector-length (js-var "prelude_vectorLength"))

;;; (vector-ref v n) -> any
;;;  v : vector?
;;;  n : integer?
;;;   a valid index into v
;;; Returns the value at index \`n\` of vector \`v\`.
;;; @category vectors, index-of, length, range, string-length, vector-length, vector-range
(define-export vector-ref (js-var "prelude_vectorRef"))

;;; (vector-set! v n x) -> void?
;;;  v : vector?
;;;  n : integer?
;;;   a valid index into v
;;;  x : any
;;; Sets the value at index \`n\` of vector \`v\` to \`x\`.
;;; @category vectors, mutation, predicates, vector-append, vector-fill!, vector-filter, vector-for-each, vector-map, vector-map!
(define-export vector-set! (js-var "prelude_vectorSet"))

;;; (vector-fill! v x) -> void?
;;;  v : vector?
;;;  x : any
;;; Sets each element of vector \`v\` to \`x\`.
;;; @category vectors, mutation, predicates, vector-append, vector-filter, vector-for-each, vector-map, vector-map!, vector-set!
(define-export vector-fill! (js-var "prelude_vectorFill"))

;;; (vector->list v) -> list?
;;;  v : vector?
;;; Returns a list consisting of the values in vector \`v\`.
;;; @category list, list creation, vectors, list->vector, vector->string
(define-export vector->list (js-var "prelude_vectorToList"))

;;; (list->vector l) -> vector?
;;;  l : list?
;;; Returns a vector consisting of the values in list \`l\`.
;;; @category list, list manipulation, association list, vectors, list->vector, vector->list
(define-export list->vector (js-var "prelude_listToVector"))

;;; (vector-range & args) -> vector?
;;;  args : integer?
;;; Can be called with one, two, or three arguments, all of which are integers.
;;; (vector-range end) returns a vector containing the numbers from 0 to \`end\` (exclusive).
;;; (vector-range beg end) returns a vector containing the numbers from \`beg\` to \`end\`
;;; (exclusive). (vector-range beg end step) returns a vector containing the numbers from
;;; \`beg\` to \`end\` (exclusive) with a step size of \`step\`. \`step\` must be non-zero
;;; to avoid an infinite loop.
;;; @category vectors,  index-of, length, range, string-length, vector-length, vector-ref 
(define-export vector-range (js-var "prelude_vectorRange"))

;;; (vector-append & v1) -> vector?
;;;  v1 : vector?
;;; Returns a new vector containing the elements of \`v1\`, ..., \`vk\` in order.
;;; @category vectors, vector-fill!, vector-filter, vector-for-each, vector-map, vector-map!, vector-set!
(define-export vector-append (js-var "prelude_vectorAppend"))

;;; (procedure? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a procedure.
;;; @category typecheck, predicates, pair?, list?, null?, ref?, rex?, vector?, void?
(define-export procedure? (js-var "prelude_procedureQ"))

;;; (string-map f s) -> string?
;;;  f : procedure?
;;;  s : string?
;;; Returns a new string containing the results of applying \`f\` to each character of \`s\`.
;;; @category string, make-string, string-append, map, vector-map, vector-map!
(define-export string-map
  (lambda (f s)
    (list->string (map f (string->list s)))))

;; N.B., internal helpers for n-ary map: given a list of lists, collect the
;; cars (heads) or cdrs (tails) of each. Undocumented (like all-satisfy?) so
;; the contract-check codegen leaves them alone.
(define-export lists-cars
  (lambda (lsts)
    (if (null? lsts)
        null
        (cons (car (car lsts)) (lists-cars (cdr lsts))))))

(define-export lists-cdrs
  (lambda (lsts)
    (if (null? lsts)
        null
        (cons (cdr (car lsts)) (lists-cdrs (cdr lsts))))))

;; N.B., the \`-onto\` helpers below (map-onto, filter-onto, fold-right-onto)
;; exist so that map, filter, fold-right, and reduce-right recurse in *tail*
;; position. Written naively they held one live frame per element and blew
;; past Fiber.maxCallStackDepth at ~10,000 elements -- a 100x100 image (#453).
;;
;; Two rules govern where such a helper may go, both learned the hard way:
;;
;;   1. Define it at the top level, and never as a \`let\`-bound lambda inside
;;      the function it serves. A tail call *replaces* the caller's frame, so
;;      tail-calling a helper built at call time pops the frame that was hiding
;;      the library's insides. That used to spill map's \`cond\` into the
;;      student's reduction trace: a closure built at call time was not marked
;;      as library code. Since #476 it is (a closure takes the origin of the
;;      frame that built it -- see ClsHandler), and since #478 the shield
;;      survives the tail call itself (see Frame.hidden), so the machine now
;;      holds this rather than the rule; keeping the helpers at the top level
;;      is still clearer.
;;   2. Put it *above* the neighbouring \`;;;\` docstring block, never between
;;      that block and the function it documents -- a docstring binds to the
;;      define directly below it. Since #479 the compiler checks this, so
;;      getting it wrong stops the library from loading rather than handing
;;      the helper someone else's contract.
;;
;; They are undocumented (like lists-cars above) so the codegen leaves them
;; alone, which also means map's own contract is checked once per call rather
;; than once per element.
(define-export map-onto
  (lambda (f lsts acc)
    (cond
      ;; N.B., the \`(null? lsts)\` disjunct is what makes this total: with no
      ;; lists at all there is nothing to take a cdr of, and all-satisfy? is
      ;; vacuously true, so \`(map f)\` lands here and yields null.
      [(or (null? lsts) (some-satisfy? null? lsts))
       (if (all-satisfy? null? lsts)
           (reverse acc)
           (error "map: all lists must have the same length"))]
      [else (map-onto f (lists-cdrs lsts)
                      (cons (apply f (lists-cars lsts)) acc))])))

;;; (map f & l) -> list?
;;;  f : procedure?
;;;  l : list?
;;; Returns a new list containing the results of applying \`f\` to each element of \`l\`. When several lists are given, \`f\` is applied element-wise across them and all lists must have the same length.
;;; @category list, list manipulation, association list, reduce, reduce-right, set-maximum-recursion-depth!, string-map, vector-map, vector-map!
(define-export map
  (lambda (f & lsts)
    (map-onto f lsts null)))

; N.B., filter's tail-recursive worker; see the note above map-onto.
(define-export filter-onto
  (lambda (f l acc)
    (cond
      [(null? l) (reverse acc)]
      [(f (car l)) (filter-onto f (cdr l) (cons (car l) acc))]
      [else (filter-onto f (cdr l) acc)])))

;;; (filter f l) -> list?
;;;  f : procedure?
;;;  l : list?
;;; Returns a new list containing the elements of \`l\` for which \`f\` returns \`#t\`.
;;; @category list, list manipulation, association list, apply, fold, fold-left, fold-right, for-range, list-of, map, reduce, reduce-right
(define-export filter
  (lambda (f l)
    (filter-onto f l null)))

;;; (fold f v l) -> any
;;;  f : procedure?
;;;  v : any
;;;  l : list?
;;; Returns the result of accumulating the result of applying \`f\` to each element of \`l\`, starting with initial value \`v\`. The function \`f\` takes two arguments, the first is the accumulated value and the second is the current element.
;;; @category list, list manipulation, association list, fold-left, fold-right, for-range, list-of, map, reduce, reduce-right, apply, filter
(define-export fold
  (lambda (f v l)
    (if (null? l)
        v
        (fold f (f v (car l)) (cdr l)))))

;;; (reduce f l) -> any
;;;  f : procedure?
;;;  l : list?
;;; Like \`fold\` but uses the first element of \`l\` as the initial value.
;;; @category list, list manipulation, reduce-right, apply, filter, fold, fold-left, fold-right, for-range, list-of, map, set-maximum-recursion-depth!
(define-export reduce
  (lambda (f l)
    (fold f (car l) (cdr l))))

;;; (fold-left f v l) -> any
;;;  f : procedure?
;;;  v : any
;;;  l : list?
;;; Like \`fold\`, but the combining function \`f\` takes the current element as its first argument and the accumulated value as its second.
;;; @category list, list manipulation, association list, fold, fold-right, for-range, list-of, map, reduce, reduce-right, apply, filter
(define-export fold-left
  (lambda (f v l)
    (if (null? l) v (fold-left f (f (car l) v) (cdr l)))))

; N.B., fold-right's and reduce-right's tail-recursive worker: \`rev\` is the
; list reversed, so walking it forwards combines from the right end inwards --
; and so \`f\` is applied to the rightmost element first, exactly as the naive
; nesting did. See the note above map-onto.
(define-export fold-right-onto
  (lambda (f acc rev)
    (if (null? rev)
        acc
        (fold-right-onto f (f (car rev) acc) (cdr rev)))))

;;; (fold-right f v l) -> any
;;;  f : procedure?
;;;  v : any
;;;  l : list?
;;; Returns the result of accumulating the result of applying \`f\` to each element of \`l\` in reverse order, starting with initial value \`v\`. The function \`f\` takes two arguments, the first is the current element and the second is the accumulated value.
;;; @category list, list manipulation, association list, fold, fold-left, for-range, list-of, map, reduce, reduce-right, apply, filter
(define-export fold-right
  (lambda (f v l)
    (fold-right-onto f v (reverse l))))

;;; (reduce-right f l) -> any
;;;  f : procedure?
;;;  l : list?
;;; Like \`fold-right\` but uses the last element of \`l\` as the initial value.
;;; @category list, list manipulation, range, apply, filter, fold, fold-left, fold-right, for-range, list-of, map, set-maximum-recursion-depth!
(define-export reduce-right
  (lambda (f l)
    (match (reverse l)
      [(cons x null) x]
      [(cons x rest) (fold-right-onto f x rest)])))

;;; (vector-map f & v) -> vector?
;;;  f : procedure?
;;;  v : vector?
;;; Returns a new vector containing the results of applying \`f\` to each element of \`v1\`, ..., \`vk\` in a element-wise fashion.
;;; @category vectors, map, string-map, vector-append, vector-fill!, vector-filter, vector-for-each, vector-map!, vector-set!
(define-export vector-map
  (lambda (f & vs)
    (match vs
      ;; The one-vector case -- overwhelmingly the common one, and the one
      ;; \`pixel-map\` drives -- fills a result vector directly, rather than
      ;; routing a whole image through a list and back (#453). The k-vector
      ;; case still does, since \`map\` is what defines element-wise behavior
      ;; across several vectors.
      [(cons v null)
       (let ([result (make-vector (vector-length v) void)])
         (begin
           (for-range (lambda (i) (vector-set! result i (f (vector-ref v i))))
             0 (vector-length v))
           result))]
      [_ (list->vector (apply map (cons f (map vector->list vs))))])))

;;; (vector-map! f v) -> void?
;;;  f : procedure?
;;;  v : vector?
;;; Mutates \`v\` in place, replacing each element with the result of applying \`f\` to it.
;;; @category vectors, mutation, predicates, map, string-map, vector-append, vector-fill!, vector-filter, vector-for-each, vector-map, vector-set!
(define-export vector-map!
  (lambda (f v)
    (for-range (lambda (i) (vector-set! v i (f (vector-ref v i))))
      0 (vector-length v))))

;;; (vector-for-each f v) -> void?
;;;  f : procedure?
;;;  v : vector?
;;; Runs \`f\` on each element of \`v\` in order, for its side effects.
;;; @category vectors, vector-append, vector-fill!, vector-filter, vector-map, vector-map!, vector-set!
(define-export vector-for-each
  (lambda (f v)
    (for-range (lambda (i) (f (vector-ref v i)))
      0 (vector-length v))))

;;; (for-range f beg end) -> void?
;;;  f : procedure?
;;;  beg : number?
;;;  end : number?
;;; Runs \`f\` on each integer in the range \`[beg, end)\`. \`f\` takes one argument, the current value of integer.
;;; @category other, fold-left, fold-right, list-of, map, reduce, reduce-right, apply, filter
(define-export for-range
  (lambda (f beg end)
    (cond
      [(< beg end) (begin (f beg) (for-range f (+ beg 1) end))]
      [(> beg end) (begin (f beg) (for-range f (- beg 1) end))]
      [else void])))

;;; (vector-filter f v) -> vector?
;;;  f : procedure?
;;;  v : vector?
;;; Returns a new vector containing the elements of \`v\` for which \`f\` returns \`#t\`.
;;; @category vectors, vector-append, vector-fill!, vector-for-each, vector-map, vector-map!, vector-set!
(define-export vector-filter
  (lambda (f v)
    (list->vector (filter f (vector->list v)))))

;;; (void? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is the void value.
;;; @category predicates, typecheck, pair?, list?, null?, procedure?, ref?, vector?
(define-export void? (js-var "prelude_voidQ"))

;;; (compose & f1) -> procedure?
;;;  f1 : procedure?
;;; Returns a new procedure that is the composition of the given functions, _i.e._, \`f(x) = f1(f2(...(fk(x))))\`.
;;; @category function composition, all-of, any-of, =-eps, o, |>
(define-export compose
  (lambda (& fs)
    (lambda (x)
      (fold-right (lambda (g acc) (g acc)) x fs))))

;;; (o & f) -> procedure?
;;;  f : procedure?
;;; A synonym for \`compose\`.
;;; @category function composition, all-of, any-of, compose, =-eps, |>
(define-export o
  (lambda (& fs)
    (apply compose fs)))

;;; (|> v & f1) -> any
;;;  v : any
;;;  f1 : procedure?
;;; Returns the result of applying the given function in sequence, starting with initial value \`v\`, _i.e._, \`(fk (fk-1(...(f1 v)))\`.
;;; @category function composition, all-of, any-of, compose, =-eps, o
(define-export |>
  (lambda (v & fs)
    (fold (lambda (acc f) (f acc)) v fs)))

;;; (range & args) -> list?
;;;  args : integer?
;;; Can be called with one, two, or three arguments, all of which are integers.
;;; (range end) returns a list containing the numbers from 0 to \`end\` (exclusive).
;;; (range beg end) returns a list containing the numbers from \`beg\` to \`end\`
;;; (exclusive). (range beg end step) returns a list containing the numbers from
;;; \`beg\` to \`end\` (exclusive) with a step size of \`step\`. \`step\` must be non-zero
;;; to avoid an infinite loop.
;;; @category list, list creation, append, list-drop, list-tail, list-take, make-list, reverse, sort, index-of, length, string-length, vector-length, vector-range, vector-ref 
(define-export range (js-var "prelude_range"))

;;; (random n) -> number?
;;;  n : integer?
;;;   n >= 0
;;; Returns a random number in the range 0 to n (exclusive).
;;; @category other
(define-export random (js-var "prelude_random"))

;; N.B., \`with-handler\` is now a reserved-word special form, not a library
;; binding -- it must install an exception handler in the bytecode, which a
;; js-var-backed procedure cannot do. Its handler/function/args are ordinary
;; sub-expressions; on a raised runtime error the fiber unwinds to the handler
;; and applies it to the error's message string. Parsing lives in
;; syntax.grammar (WithHandler) + lezer-bridge; the AST node is WithHandlerExp;
;; codegen lowers it to push-handler/apply/pop-handler (see the LPM handler
;; stack in src/lpm/fiber.ts). As it is no longer a documented define, it does
;; not appear on the generated docs site (like \`error\`/\`if\`/\`cond\`).

;;; (ignore v) -> void?
;;;  v : any
;;; Suppresses the output of value \`v\` to the page.
;;; @category other
(define-export ignore (js-var "prelude_ignore"))

;;; (set-maximum-recursion-depth! n) -> void?
;;;  n : integer?
;;;   a whole number between 1 and 200000
;;; Sets the maximum recursion depth of Scamper to \`n\`, in effect until the program is run again. Note that tail call-optimized functions do _not_ count towards this limit. Each level of recursion costs memory, so a very deep limit may exhaust it before it is reached.
;;; @category mutation, predicates, map, reduce, reduce-right
(define-export set-maximum-recursion-depth! (js-var "prelude_setMaximumRecursionDepth"))

;;; (string->words s) -> list?
;;;  s : string?
;;; Returns a list of the words in \`s\`, stripping whitespace and punctuation.
;;; @category string, string->list, string->number, string->vector, string->chars, string->lines
(define-export string->words (js-var "prelude_stringToWords"))

;;; (ref v) -> ref?
;;;  v : any
;;; Returns a reference cell initially containing \`v\`.
;;; @category other
(define-export ref (js-var "prelude_ref"))

;;; (ref? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only \`v\` is a reference cell.
;;; @category typecheck, predicates, pair?, list?, null?, procedure?, rex?, vector?, void?
(define-export ref? (js-var "prelude_isRef"))

;;; (deref r) -> any
;;;  r : ref?
;;; Returns the value contained in reference cell \`r\`.
;;; @category other, assoc-ref, list-ref, ref-set!, string-ref
(define-export deref (js-var "prelude_deref"))

;;; (ref-set! r v) -> void?
;;;  r : ref?
;;;  v : any
;;; Sets the value contained in reference cell \`r\` to \`v\`.
;;; @category mutation, assoc-ref, deref, list-ref, string-ref
(define-export ref-set! (js-var "prelude_refSet"))

;;; (hash? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a map, the kind of value a \`{ ... }\` literal produces.
;;; @category hashmap, typecheck, predicates, hash-ref, hash-set, hash-keys
(define-export hash? (js-var "prelude_hashQ"))

;;; (hash-ref h k) -> any
;;;  h : hash?
;;;  k : string?
;;; Returns the value that map \`h\` associates with key \`k\`. Raises an error if \`h\` has no such key; use \`hash-ref-or\` to supply a default instead.
;;; @category hashmap, hash-ref-or, hash-has-key?, hash-set, hash-keys
(define-export hash-ref (js-var "prelude_hashRef"))

;;; (hash-ref-or h k default) -> any
;;;  h : hash?
;;;  k : string?
;;;  default : any
;;; Returns the value that map \`h\` associates with key \`k\`, or \`default\` if \`h\` has no such key.
;;; @category hashmap, hash-ref, hash-has-key?, hash-set
(define-export hash-ref-or (js-var "prelude_hashRefOr"))

;;; (hash-has-key? h k) -> boolean?
;;;  h : hash?
;;;  k : string?
;;; Returns \`#t\` if and only if map \`h\` associates a value with key \`k\`.
;;; @category hashmap, typecheck, predicates, hash-ref, hash-ref-or, hash-keys
(define-export hash-has-key? (js-var "prelude_hashHasKeyQ"))

;;; (hash-set h k v) -> hash?
;;;  h : hash?
;;;  k : string?
;;;  v : any
;;; Returns a new map like \`h\` but with key \`k\` associated with \`v\`. \`h\` itself is unchanged.
;;; @category hashmap, hash-remove, hash-ref, hash-count
(define-export hash-set (js-var "prelude_hashSet"))

;;; (hash-set! h k v) -> void?
;;;  h : hash?
;;;  k : string?
;;;  v : any
;;; Mutates map \`h\` in place, associating key \`k\` with \`v\`. Unlike \`hash-set\`, no new map is made, so every binding that refers to \`h\` sees the change.
;;; @category hashmap, mutation, hash-set, hash-ref, hash-remove
(define-export hash-set! (js-var "prelude_hashSetBang"))

;;; (hash-remove h k) -> hash?
;;;  h : hash?
;;;  k : string?
;;; Returns a new map like \`h\` but with key \`k\` removed. \`h\` itself is unchanged, and removing a key that is not present is not an error.
;;; @category hashmap, hash-set, hash-ref, hash-count
(define-export hash-remove (js-var "prelude_hashRemove"))

;;; (hash-count h) -> integer?
;;;  h : hash?
;;; Returns the number of key-value pairs in map \`h\`.
;;; @category hashmap, hash-keys, hash-values, hash-set
(define-export hash-count (js-var "prelude_hashCount"))

;;; (hash-keys h) -> list?
;;;  h : hash?
;;; Returns a list of the keys of map \`h\`.
;;; @category hashmap, hash-values, hash->list, hash-count, hash-has-key?
(define-export hash-keys (js-var "prelude_hashKeys"))

;;; (hash-values h) -> list?
;;;  h : hash?
;;; Returns a list of the values of map \`h\`, in the same order as \`hash-keys\`.
;;; @category hashmap, hash-keys, hash->list, hash-count
(define-export hash-values (js-var "prelude_hashValues"))

;;; (hash->list h) -> list?
;;;  h : hash?
;;; Returns the contents of map \`h\` as a list of key-value pairs.
;;; @category hashmap, list->hash, hash-keys, hash-values
(define-export hash->list (js-var "prelude_hashToList"))

;;; (list->hash l) -> hash?
;;;  l : list?
;;; Returns a map built from \`l\`, a list of key-value pairs whose keys are strings. If a key appears more than once, the last pair wins.
;;; @category hashmap, hash->list, hash-set, hash-keys
(define-export list->hash (js-var "prelude_listToHash"))

;;; else: boolean?
;;; A synonym for \`#t\` appropriate for use as the final guard of a \`cond\` expression.
;;; @category boolean/logic, constants
(define-export else (js-var "prelude_elseConst"))

;;; null: list?
;;; The empty list.
;;; @category list, list creation, association list, constants
(define-export null (js-var "prelude_nullConst"))

;;; pi: number?
;;; The constant π.
;;; @category math, algebra, constants, acos, asin, atan, cos, sin, tan, π  
(define-export pi (js-var "prelude_piConst"))

;;; π: number?
;;; The constant π.
;;; @category math, algebra, constants, acos, asin, atan, cos, sin, tan, pi  
(define-export π (js-var "prelude_piConst"))

;;; void: void?
;;; The void value.
;;; @category constants, list, pair, vector
(define-export void (js-var "prelude_voidConst"))

;;; (with-file filename fn) -> any
;;;  filename : string?
;;;  fn : procedure?
;;; Loads \`filename\` from storage and passes its contents to \`fn\` as input. The output of \`fn\` is returned (and rendered to the screen if this is a top-level expression).
;;; @category other, with-file-chooser, with-handler, file->lines, file->string
(define-export with-file
  (lambda (filename fn)
    (fn ((js-var "prelude_blockOnReadFile") filename))))

;;; (with-file-chooser fn) -> void?
;;;  fn : procedure?
;;; Renders a file chooser widget. When the user selects a file, its contents are passed to \`fn\` as input. The output of \`fn\` is then rendered to the screen.
;;; @category interactive, with-file, with-handler, file->lines, file->string
(define-export with-file-chooser (js-var "prelude_withFileChooser"))

;;; (caar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (car v))\`.
;;; @category list, list manipulation, association list
(define-export caar (lambda (v) (car (car v))))

;;; (cadr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (cdr v))\`.
;;; @category list, list manipulation, association list
(define-export cadr (lambda (v) (car (cdr v))))

;;; (cdar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (car v))\`.
;;; @category list, list manipulation, association list
(define-export cdar (lambda (v) (cdr (car v))))

;;; (cddr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (cdr v))\`.
;;; @category list, list manipulation, association list
(define-export cddr (lambda (v) (cdr (cdr v))))

;;; (caaar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (car (car v)))\`.
;;; @category list, list manipulation, association list
(define-export caaar (lambda (v) (car (car (car v)))))

;;; (cadar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (cdr (car v)))\`.
;;; @category list, list manipulation, association list
(define-export cadar (lambda (v) (car (cdr (car v)))))

;;; (cdaar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (car (car v)))\`.
;;; @category list, list manipulation, association list
(define-export cdaar (lambda (v) (cdr (car (car v)))))

;;; (cddar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (cdr (car v)))\`.
;;; @category list, list manipulation, association list
(define-export cddar (lambda (v) (cdr (cdr (car v)))))

;;; (caadr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (car (cdr v)))\`.
;;; @category list, list manipulation, association list
(define-export caadr (lambda (v) (car (car (cdr v)))))

;;; (caddr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (cdr (cdr v)))\`.
;;; @category list, list manipulation, association list
(define-export caddr (lambda (v) (car (cdr (cdr v)))))

;;; (cdadr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (car (cdr v)))\`.
;;; @category list, list manipulation, association list
(define-export cdadr (lambda (v) (cdr (car (cdr v)))))

;;; (cdddr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (cdr (cdr v)))\`.
;;; @category list, list manipulation, association list
(define-export cdddr (lambda (v) (cdr (cdr (cdr v)))))

;;; (caaaar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (car (car (car v))))\`.
;;; @category list, list manipulation, association list
(define-export caaaar (lambda (v) (car (car (car (car v))))))

;;; (cadaar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (cdr (car (car v))))\`.
;;; @category list, list manipulation, association list
(define-export cadaar (lambda (v) (car (cdr (car (car v))))))

;;; (cdaaar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (car (car (car v))))\`.
;;; @category list, list manipulation, association list
(define-export cdaaar (lambda (v) (cdr (car (car (car v))))))

;;; (cddaar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (cdr (car (car v))))\`.
;;; @category list, list manipulation, association list
(define-export cddaar (lambda (v) (cdr (cdr (car (car v))))))

;;; (caadar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (car (cdr (car v))))\`.
;;; @category list, list manipulation, association list
(define-export caadar (lambda (v) (car (car (cdr (car v))))))

;;; (caddar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (cdr (cdr (car v))))\`.
;;; @category list, list manipulation, association list
(define-export caddar (lambda (v) (car (cdr (cdr (car v))))))

;;; (cdadar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (car (cdr (car v))))\`.
;;; @category list, list manipulation, association list
(define-export cdadar (lambda (v) (cdr (car (cdr (car v))))))

;;; (cdddar v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (cdr (cdr (car v))))\`.
;;; @category list, list manipulation, association list
(define-export cdddar (lambda (v) (cdr (cdr (cdr (car v))))))

;;; (caaadr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (car (car (cdr v))))\`.
;;; @category list, list manipulation, association list
(define-export caaadr (lambda (v) (car (car (car (cdr v))))))

;;; (cadadr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (cdr (car (cdr v))))\`.
;;; @category list, list manipulation, association list
(define-export cadadr (lambda (v) (car (cdr (car (cdr v))))))

;;; (cdaadr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (car (car (cdr v))))\`.
;;; @category list, list manipulation, association list
(define-export cdaadr (lambda (v) (cdr (car (car (cdr v))))))

;;; (cddadr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (cdr (car (cdr v))))\`.
;;; @category list, list manipulation, association list
(define-export cddadr (lambda (v) (cdr (cdr (car (cdr v))))))

;;; (caaddr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (car (cdr (cdr v))))\`.
;;; @category list, list manipulation, association list
(define-export caaddr (lambda (v) (car (car (cdr (cdr v))))))

;;; (cadddr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(car (cdr (cdr (cdr v))))\`.
;;; @category list, list manipulation, association list
(define-export cadddr (lambda (v) (car (cdr (cdr (cdr v))))))

;;; (cdaddr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (car (cdr (cdr v))))\`.
;;; @category list, list manipulation, association list
(define-export cdaddr (lambda (v) (cdr (car (cdr (cdr v))))))

;;; (cddddr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Equivalent to \`(cdr (cdr (cdr (cdr v))))\`.
;;; @category list, list manipulation, association list
(define-export cddddr (lambda (v) (cdr (cdr (cdr (cdr v))))))

;;; (char=? & c1) -> boolean?
;;;  c1 : char?
;;; Returns \`#t\` if and only \`c1\`, \`c2\`, ... are all equivalent characters.
;;; @category char, predicates, char>=?, char>?, char<=?, char<?
(define-export char=? (js-var "prelude_char=?"))

;;; (char<? & c1) -> boolean?
;;;  c1 : char?
;;; Returns \`#t\` if and only \`c1\`, \`c2\`, ... have strictly increasing character values.
;;; @category char, predicates, char=?, char>=?, char>?, char<=?
(define-export char<? (js-var "prelude_char<?"))

;;; (char>? & c1) -> boolean?
;;;  c1 : char?
;;; Returns \`#t\` if and only \`c1\`, \`c2\`, ... have strictly decreasing character values.
;;; @category char, predicates, char=?, char>=?, char<=?, char<?
(define-export char>? (js-var "prelude_char>?"))

;;; (char<=? & c1) -> boolean?
;;;  c1 : char?
;;; Returns \`#t\` if and only \`c1\`, \`c2\`, ... have non-decreasing character values.
;;; @category char, predicates, char=?, char>=?, char>?, char<?
(define-export char<=? (js-var "prelude_char<=?"))

;;; (char>=? & c1) -> boolean?
;;;  c1 : char?
;;; Returns \`#t\` if and only \`c1\`, \`c2\`, ... have non-increasing character values.
;;; @category char, predicates, char=?, char>?, char<=?, char<?
(define-export char>=? (js-var "prelude_char>=?"))

;;; (char-ci=? & c1) -> boolean?
;;;  c1 : char?
;;; Returns \`#t\` if and only \`c1\`, \`c2\`, ... are all equivalent characters, ignoring case.
;;; @category char, predicates, char-ci>=?, char-ci>?, char-ci<=?, char-ci<?
(define-export char-ci=? (js-var "prelude_char-ci=?"))

;;; (char-ci<? & c1) -> boolean?
;;;  c1 : char?
;;; Returns \`#t\` if and only \`c1\`, \`c2\`, ... have strictly increasing character values, ignoring case.
;;; @category char, predicates, char-ci=?, char-ci>=?, char-ci>?, char-ci<=?
(define-export char-ci<? (js-var "prelude_char-ci<?"))

;;; (char-ci>? & c1) -> boolean?
;;;  c1 : char?
;;; Returns \`#t\` if and only \`c1\`, \`c2\`, ... have strictly decreasing character values, ignoring case.
;;; @category char, predicates, char-ci=?, char-ci>=?, char-ci<=?, char-ci<?
(define-export char-ci>? (js-var "prelude_char-ci>?"))

;;; (char-ci<=? & c1) -> boolean?
;;;  c1 : char?
;;; Returns \`#t\` if and only \`c1\`, \`c2\`, ... have non-decreasing character values, ignoring case.
;;; @category char, predicates, char-ci=?, char-ci>=?, char-ci>?, char-ci<?
(define-export char-ci<=? (js-var "prelude_char-ci<=?"))

;;; (char-ci>=? & c1) -> boolean?
;;;  c1 : char?
;;; Returns \`#t\` if and only \`c1\`, \`c2\`, ... have non-increasing character values, ignoring case.
;;; @category char, predicates, char-ci=?, char-ci>?, char-ci<=?, char-ci<?
(define-export char-ci>=? (js-var "prelude_char-ci>=?"))

;;; (char-alphabetic? c) -> boolean?
;;;  c : char?
;;; Returns \`#t\` if and only \`c\` is an alphabetic character.
;;; @category typecheck, char, predicates, char-numeric?, char-lower-case?, char-upper-case?, char-whitespace?
(define-export char-alphabetic? (js-var "prelude_char-alphabetic?"))

;;; (char-numeric? c) -> boolean?
;;;  c : char?
;;; Returns \`#t\` if and only \`c\` is a numeric character.
;;; @category typecheck, char, predicates, char-alphabetic?, char-lower-case?, char-upper-case?, char-whitespace?
(define-export char-numeric? (js-var "prelude_char-numeric?"))

;;; (char-whitespace? c) -> boolean?
;;;  c : char?
;;; Returns \`#t\` if and only \`c\` is a whitespace character.
;;; @category typecheck, char, predicates, char-alphabetic?, char-numeric?, char-lower-case?, char-upper-case?
(define-export char-whitespace? (js-var "prelude_char-whitespace?"))

;;; (char-upper-case? c) -> boolean?
;;;  c : char?
;;; Returns \`#t\` if and only \`c\` is an upper-case character.
;;; @category typecheck, char, predicates, char-alphabetic?, char-numeric?, char-lower-case?, char-whitespace?
(define-export char-upper-case? (js-var "prelude_char-upper-case?"))

;;; (char-lower-case? c) -> boolean?
;;;  c : char?
;;; Returns \`#t\` if and only \`c\` is a lower-case character.
;;; @category char, predicates, char-alphabetic?, char-numeric?, char-upper-case?, char-whitespace?
(define-export char-lower-case? (js-var "prelude_char-lower-case?"))

;;; (string=? & s1) -> boolean?
;;;  s1 : string?
;;; Returns \`#t\` if and only \`s1\`, \`s2\`, ... are equivalent strings.
;;; @category string, predicates, string>=?, string>?, string<=?, string<?, string-contains
(define-export string=? (js-var "prelude_string=?"))

;;; (string<? & s1) -> boolean?
;;;  s1 : string?
;;; Returns \`#t\` if and only \`s1\`, \`s2\`, ... are in strictly lexicographically increasing order.
;;; @category string, predicates, string=?, string>=?, string>?, string<=?, string-contains
(define-export string<? (js-var "prelude_string<?"))

;;; (string>? & s1) -> boolean?
;;;  s1 : string?
;;; Returns \`#t\` if and only \`s1\`, \`s2\`, ... are in strictly lexicographically decreasing order.
;;; @category string, predicates, string=?, string>=?, string<=?, string<?, string-contains
(define-export string>? (js-var "prelude_string>?"))

;;; (string<=? & s1) -> boolean?
;;;  s1 : string?
;;; Returns \`#t\` if and only \`s1\`, \`s2\`, ... are in lexicographical order.
;;; @category string, predicates, string=?, string>=?, string>?, string<?, string-contains
(define-export string<=? (js-var "prelude_string<=?"))

;;; (string>=? & s1) -> boolean?
;;;  s1 : string?
;;; Returns \`#t\` if and only \`s1\`, \`s2\`, ... are in reverse lexicographical order.
;;; @category string, predicates, string=?, string>?, string<=?, string<?, string-contains
(define-export string>=? (js-var "prelude_string>=?"))

;;; (string-ci=? & s1) -> boolean?
;;;  s1 : string?
;;; Returns \`#t\` if and only \`s1\`, \`s2\`, ... are equivalent strings, ignoring case.
;;; @category string, predicates, string-ci>=?, string-ci>?, string-ci<=?, string-ci<?, string-contains
(define-export string-ci=? (js-var "prelude_string-ci=?"))

;;; (string-ci<? & s1) -> boolean?
;;;  s1 : string?
;;; Returns \`#t\` if and only \`s1\`, \`s2\`, ... are in strictly lexicographically increasing order, ignoring case.
;;; @category string, predicates, string-ci=?, string-ci>=?, string-ci>?, string-ci<=?, string-contains
(define-export string-ci<? (js-var "prelude_string-ci<?"))

;;; (string-ci>? & s1) -> boolean?
;;;  s1 : string?
;;; Returns \`#t\` if and only \`s1\`, \`s2\`, ... are in strictly lexicographically decreasing order, ignoring case.
;;; @category string, predicates, string-ci=?, string-ci>=?, string-ci<=?, string-ci<?, string-contains
(define-export string-ci>? (js-var "prelude_string-ci>?"))

;;; (string-ci<=? & s1) -> boolean?
;;;  s1 : string?
;;; Returns \`#t\` if and only \`s1\`, \`s2\`, ... are in lexicographical order, ignoring case.
;;; @category string, predicates, string-ci=?, string-ci>=?, string-ci>?, string-ci<?, string-contains
(define-export string-ci<=? (js-var "prelude_string-ci<=?"))

;;; (string-ci>=? & s1) -> boolean?
;;;  s1 : string?
;;; Returns \`#t\` if and only \`s1\`, \`s2\`, ... are in reverse lexicographical order, ignoring case.
;;; @category string, predicates, string-ci=?, string-ci>?, string-ci<=?, string-ci<?, string-contains
(define-export string-ci>=? (js-var "prelude_string-ci>=?"))
`],[`reactive`,`;;; Scamper's reactive functional programming library

;;; (html? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is an HTML element.
(define-export html? (js-var "html_isElement"))

;;; (button? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a button.
(define-export button? (js-var "html_buttonQ"))

;;; (subscription? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a subscription.
;;; @category reactive, html?, canvas?
(define-export subscription? (js-var "reactive_subscriptionQ"))

;;; (reactive-canvas width height initial view update & sub1) -> html?
;;;  width : number?
;;;  height : number?
;;;  initial : any
;;;  view : any
;;;   function? takes a state and canvas as input and renders that state to the canvas.
;;;  update : any
;;;   function? takes a message and state as input and returns a new state as output.
;;;  sub1 : subscription?
;;; Creates a reactive canvas with the given width and height. The canvas renders as model with the given \`initial\` value and \`view\` function. The \`update\` function handles all messages generated by the given subscriptions.
;;; @category reactive, canvas, html, html?, reactive-container
(define-export reactive-canvas (js-var "reactive_reactiveCanvas"))

;;; (reactive-container initial view update & sub1) -> html?
;;;  initial : any
;;;  view : any
;;;   function? takes a state as input and outputs an HTML element as output.
;;;  update : any
;;;   function? takes a message and state as input and returns a new state as output.
;;;  sub1 : subscription?
;;; Creates a reactive HTML container. The canvas renders as model with the given \`initial\` value and \`view\` function. The \`view\` function outputs an HTML tree that the container displays. The \`update\` function handles all messages generated by the given subscriptions.
;;; @category reactive, html, html?, reactive-canvas
(define-export reactive-container (js-var "reactive_reactiveContainer"))

;;; (on-button-click button) -> subscription?
;;;  button : button?
;;; Returns a subscription that emits a message of the form \`(event-button-click id)\` whenever the given button is clicked. The \`id\` is the given button's id or \`void\` is no \`id\` assigned to the button.
;;; @category reactive, subscription?, on-mouse-click, on-mouse-hover
(define-export on-button-click (js-var "reactive_onButtonClick"))

;;; (on-mouse-click) -> subscription?
;;; Returns a subscription that emits a message of the form \`(event-mouse-click button x y)\` whenever the canvas is clicked. \`button\` indicates which mouse button was clicked and \`(x, y)\` are the coordinates where the click occurred.
;;; @category reactive, subscription?, on-button-click, on-mouse-hover
(define-export on-mouse-click (js-var "reactive_onMouseClick"))

;;; (on-mouse-hover) -> subscription?
;;; Returns a subscription that emits a message of the form \`(event-mouse-hover x y)\` whenever the mouse moves over the reactive element. \`(x, y)\` are the coordinates of the mouse.
;;; @category reactive, subscription?, on-button-click, on-mouse-click
(define-export on-mouse-hover (js-var "reactive_onMouseHover"))

;;; (on-key-down) -> subscription?
;;; Returns a subscription that emits a message of the form \`(event-key-down key)\` whenever a key is pressed. \`key\` indicates the key that was pressed.
;;; @category reactive, subscription?, on-key-up
(define-export on-key-down (js-var "reactive_onKeyDown"))

;;; (on-key-up) -> subscription?
;;; Returns a subscription that emits a message of the form \`(event-key-up key)\` whenever a key is released. \`key\` indicates the key that was released.
;;; @category reactive, subscription?, on-key-down
(define-export on-key-up (js-var "reactive_onKeyUp"))

;;; (on-timer interval) -> subscription?
;;;  interval : any
;;;   non-negative integer
;;; Returns a subscription that emits a message of the form \`(event-timer time elapsed)\` every \`interval\` milliseconds. \`time\` is the current time since the page was loaded and \`elapsed\` is the time since the last timer message, all in milliseconds.
;;; @category reactive, subscription?, on-note
(define-export on-timer (js-var "reactive_onTimer"))

;;; (on-note handlers) -> subscription?
;;;  handlers : vector?
;;; Returns a subscription that emits a message of the form \`(event-note id)\` whenever \`(note-event id)\` is played in a composition that users \`handlers\` to handle events.
;;; @category reactive, subscription?, on-timer
(define-export on-note (js-var "reactive_onNote"))
`],[`rex`,`;;; Regular expression functions

;;; (rex? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if \`v\` is a regex, \`#f\` otherwise.
;;; @category predicates, regexes, typecheck, rex-matches?, pair?, list?, procedure?, ref?, vector?
(define-export rex? (js-var "rex_isRegex"))

;;; (rex-empty) -> rex?
;;; Returns a regex that matches the empty string.
;;; @category regexes, rex-empty, rex-split-string, rex-string, rex->string
(define-export rex-empty (js-var "rex_rexEmpty"))

;;; (rex-string s) -> rex?
;;;  s : string?
;;; Returns a regex that matches the exact string \`s\`.
;;; @category regexes, rex-empty, rex-split-string, rex->string
(define-export rex-string (js-var "rex_rexString"))

;;; (rex-repeat r) -> rex?
;;;  r : rex?
;;; Returns a regex that matches one or more repetitions of the regex \`r\`.
;;; @category regexes, rex-concat, rex-repeat-o
(define-export rex-repeat (js-var "rex_rexRepeat"))

;;; (rex-repeat-0 r) -> rex?
;;;  r : rex?
;;; Returns a regex that matches zero or more repetitions of the regex \`r\`.
;;; @category regexes, rex-concat, rex-repeat
(define-export rex-repeat-0 (js-var "rex_rexRepeat0"))

;;; (rex-concat & xs) -> rex?
;;;  xs : rex?
;;; Returns a regex that matches the concatenation of the regexes \`rs\` in order.
;;; @category regexes, rex-repeat, rex-repeat-o
(define-export rex-concat (js-var "rex_rexConcat"))

;;; (rex-any-char) -> rex?
;;; Returns a regex that matches any single character.
;;; @category regexes, rex-char-antiset, rex-char-range, rex-char-set
(define-export rex-any-char (js-var "rex_rexAnyChar"))

;;; (rex-char-set s) -> rex?
;;;  s : string?
;;; Returns a regex that matches any single character in the string \`s\`.
;;; @category regexes, rex-any-char, rex-char-antiset, rex-char-range
(define-export rex-char-set (js-var "rex_rexCharSet"))

;;; (rex-char-antiset s) -> rex?
;;;  s : string?
;;; Returns a regex that matches any single character not in the string \`s\`.
;;; @category regexes, rex-any-char, rex-char-range, rex-char-set
(define-export rex-char-antiset (js-var "rex_rexCharAntiset"))

;;; (rex-char-range start end) -> rex?
;;;  start : char?
;;;  end : char?
;;; Returns a regex that matches any single character in the inclusive range from \`start\` to \`end\`.
;;; @category regexes, rex-any-char, rex-char-antiset, rex-char-set
(define-export rex-char-range (js-var "rex_rexCharRange"))

;;; (rex-any-of & xs) -> rex?
;;;  xs : rex?
;;; Returns a regex that matches any one of the regexes \`rs\`.
;;; @category regexes, rex-optional
(define-export rex-any-of (js-var "rex_rexAnyOf"))

;;; (rex-optional r) -> rex?
;;;  r : rex?
;;; Returns a regex that matches either the regex \`r\` or the empty string.
;;; @category regexes, rex-any-of
(define-export rex-optional (js-var "rex_rexOptional"))

;;; (regex pattern) -> rex?
;;;  pattern : string?
;;; Returns a regex that matches a Javascript regex \`pattern\`. See [the MDN documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_expressions) for more details.
;;; @category regexes, list, pair, string, vector
(define-export regex (js-var "rex_rexRegex"))

;;; (rex-find-matches r s) -> list?
;;;  r : rex?
;;;  s : string?
;;; Returns a list of all non-overlapping matches of the regex \`r\` in the string \`s\`.
;;; @category regexes, rex-matches?, rex-split-string
(define-export rex-find-matches (js-var "rex_rexFindMatches"))

;;; (rex-matches? r s) -> boolean?
;;;  r : rex?
;;;  s : string?
;;; Returns \`#t\` if the regex \`r\` matches the entire string \`s\`, \`#f\` otherwise.
;;; @category predicates, regexes, typecheck, rex-find-matches, rex?
(define-export rex-matches? (js-var "rex_rexMatches"))

;;; (rex-split-string r s) -> list?
;;;  r : rex?
;;;  s : string?
;;; Splits the string \`s\` at each match of the regex \`r\` and returns a list of the resulting substrings.
;;; @category regexes, rex-empty, rex-string, rex->string, rex-find-matches
(define-export rex-split-string (js-var "rex_rexSplitString"))

;;; (rex->string r) -> string?
;;;  r : rex?
;;; Returns the Javascript regex string representation of the regex \`r\`.
;;; @category regexes, rex-empty, rex-split-string, rex-string
(define-export rex->string (js-var "rex_rexToString"))
`],[`runtime`,`(define-export ##mkPredFn## (js-var "runtime_mkPredFn"))

(define-export ##mkCtorFn## (js-var "runtime_mkCtorFn"))

(define-export ##mkGetFn## (js-var "runtime_mkGetFn"))

(define-export ##typeOf## (js-var "runtime_typeOf"))

; Internal: builds the vector a vector literal [e1 ... ek] denotes, and the
; Javascript object a map literal {k1 v1 ... kn vn} denotes. Expansion rewrites
; every [...] / {...} into a call to these. They are internal (rather than the
; prelude's \`vector\`) so that a user binding cannot change what a literal means.
(define-export ##mkVec## (js-var "runtime_mkVec"))

(define-export ##mkObj## (js-var "runtime_mkObj"))

; Internal: raises a runtime error carrying its argument. Expansion injects it
; for a \`cond\` fall-through, and contract insertion for a failed check. It is
; internal (rather than the prelude's \`error\`) so that a user binding named
; \`error\` cannot change what those forms do.
(define-export ##error## (js-var "runtime_error"))

; Internal: aborts the running fiber and reports its argument as the answer to a
; live-evaluation query. A query wraps its target sub-expression in
; (##report## <expr>).
(define-export ##report## (js-var "runtime_report"))

; Internal: the pieces a contract wrapper needs to take a signature's optional
; parameters off its own rest parameter -- the i'th optional (or void), what
; follows the optionals, the too-many-arguments check, and the void test the
; predicate checks skip on. They are internal (rather than the prelude's \`car\`,
; \`list-tail\`, and \`void?\`) so that a documented parameter named after one of
; those cannot change what every call to its own function does.
(define-export ##optArg## (js-var "runtime_optArg"))

(define-export ##optRest## (js-var "runtime_optRest"))

(define-export ##checkArity## (js-var "runtime_checkArity"))

(define-export ##voidQ## (js-var "runtime_voidQ"))

; Internal: tags a contract wrapper with the value it wraps, so that library
; code naming this function reaches that value and skips the checks (see
; VarHandler). Contract insertion injects it; it is internal for the same
; reason the rest of this file's bindings are.
(define-export ##contracted## (js-var "runtime_contracted"))

(define-export any (js-var "runtime_any"))
`],[`test`,`;;; Scamper's unit testing functions

;;; (test-result-ok desc) -> test-result?
;;;  desc : string?
;;; Returns a test result indicating that the test named \`desc\` passed.
;;; @category testing
(define-export test-result-ok (js-var "test_testResultOk"))

;;; (test-result-error-expected desc expected actual) -> test-result?
;;;  desc : string?
;;;  expected : any
;;;  actual : any
;;; Returns a test result indicating that the test named \`desc\` failed because
;;; it produced \`actual\` instead of the \`expected\` value.
;;; @category testing
(define-export test-result-error-expected (js-var "test_testResultErrorExpected"))

;;; (test-result-error-exn desc exn) -> test-result?
;;;  desc : string?
;;;  exn : any
;;; Returns a test result indicating that the test named \`desc\` failed because
;;; it raised the unexpected exception \`exn\`.
;;; @category testing
(define-export test-result-error-exn (js-var "test_testResultErrorExn"))

;;; (test-result-error-gen desc reason) -> test-result?
;;;  desc : string?
;;;  reason : string?
;;; Returns a test result indicating that the test named \`desc\` failed for the
;;; given \`reason\`. (This constructor was formerly named \`test-error\`.)
;;; @category testing
(define-export test-result-error-gen (js-var "test_testResultErrorGeneric"))

;;; (test-result? v) -> boolean?
;;;  v : any
;;; Returns \`#t\` if and only if \`v\` is a test result.
(define-export test-result? (js-var "test_isResult"))

;;; (test-case desc eq? expected test-fn) -> test-result?
;;;  desc : string?
;;;  eq? : procedure?
;;;   a function that tests for equality between two values
;;;  expected : any
;;;  test-fn : procedure?
;;;   a function that produces the actual value to be tested
;;; Returns a test result indicating whether the given equality test passed: \`(eq? expected (test-fn))\`.
;;; @category testing
(define-export test-case
  (lambda (desc eq? expected test-fn)
    (with-handler
      (lambda (err) (test-result-error-exn desc err))
      (lambda ()
        (let ([actual (test-fn)]
              [is-equal (eq? expected actual)])
          (cond
            [(equal? is-equal #t) (test-result-ok desc)]
            [(equal? is-equal #f) (test-result-error-expected desc expected actual)]
            [else (error "Test case function should have produced a boolean")]))))))

;;; (test-exn desc test-fn) -> test-result?
;;;  desc : string?
;;;  test-fn : procedure?
;;;   a function that should throw an exception
;;; Returns a test result indicating whether the given function threw an exception.
;;; @category testing
(define-export test-exn
  (lambda (desc test-fn)
    (with-handler
      (lambda (err) (test-result-ok desc))
      (lambda ()
        (begin
          (test-fn)
          (test-result-error-gen desc "Test case did not throw an exception"))))))
`]];async function $p(e,t){let{prog:n,diagnostics:r}=await Ic(t,{insertContracts:!0,allowInternalNames:e===`runtime`});if(n===void 0||r.length>0)throw new H(`lib.loadLibrary`,`Failed to compile builtin library "${e}": ${r.map(e=>e.message).join(`; `)}`);let i=new Hi(n,Yn.empty.extendWithTopLevel([`js-var`,Bp],[`##contracted##`,Ap]),`builtin`),a=new wr(!1,!1);if(await Gc(i,{out:a,err:a}),a.errLog.length>0)throw new H(`lib.loadLibrary`,`Failed to run builtin library "${e}": ${a.errLog.join(`; `)}`);return i.addExports([`js-var`]),i.getModule()}function em(e){let t=new Map;for(let n of e){if(n.tag!==`define`&&n.tag!==`defexport`||!n.docComments)continue;let{doc:e}=Za(n.docComments);e&&t.set(n.name.name,e)}return t}var tm=new Map,nm=new Map,rm=!1;async function im(){if(!rm){for(let[e,t]of await Promise.all(Qp.map(async([e,t])=>[e,await $p(e,t)])))Li.set(e,t);for(let[e,t]of Qp){let{program:n}=Fc(t,void 0,{allowInternalNames:e===`runtime`});tm.set(e,n?em(n):new Map);let r=n?Zp(t,n):void 0;r!==void 0&&nm.set(e,r)}rm=!0}}var am=Li;export{Oo as A,_i as B,X as C,Do as D,ro as E,ea as F,oi as H,J as I,$i as L,Za as M,pa as N,ko as O,sa as P,Hi as R,es as S,io as T,si as U,ai as V,ui as W,Bs as _,Up as a,ts as b,Lc as c,_c as d,uc as f,Js as g,Us as h,qp as i,jo as j,Z as k,Rc as l,Ys as m,im as n,Vc as o,ac as p,nm as r,Ic as s,tm as t,Fc as u,bs as v,Y as w,ms as x,ps as y,Li as z};