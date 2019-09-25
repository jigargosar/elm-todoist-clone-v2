import './index.css'
import { Elm } from './Main.elm'

const app = Elm.Main.init({
  flags: { cache: localStorage.getItem("elm-todoist-clone-v2-cache") , now: Date.now()},
  node: document.getElementById('root'),
})


app.ports.setCache.subscribe(cacheString=> localStorage.setItem("elm-todoist-clone-v2-cache", cacheString))
