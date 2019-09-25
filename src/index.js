import './index.css'
import { Elm } from './Main.elm'

Elm.Main.init({
  flags: { cache: localStorage.getItem("elm-todoist-clone-v2-cache") , now: Date.now()},
  node: document.getElementById('root'),
})
