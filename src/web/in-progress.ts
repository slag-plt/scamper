// TODO: we should really move to a UI framework
export function mkInProgress(stopId: string) {
  const el = document.createElement("div")
  el.style.display = "none"

  const stopBtn = document.createElement("button")
  stopBtn.id = stopId
  stopBtn.className = "fa-solid fa-stop"
  stopBtn.setAttribute("aria-label", "Stop")
  el.appendChild(stopBtn)

  const spinner = document.createElement("div")
  spinner.className = "fa-solid fa-spinner fa-spin"
  el.appendChild(spinner)

  return {
    el,
    stopBtn,
    showProgress: () => {
      el.style.display = "inline"
    },
    hideProgress: () => {
      el.style.display = "none"
    },
  }
}
