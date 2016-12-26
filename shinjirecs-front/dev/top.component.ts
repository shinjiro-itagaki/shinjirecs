// <reference path="../src/lib/jquery.d.ts" />

import {Component} from '@angular/core'
import {provideRouter, ROUTER_DIRECTIVES, Router} from '@angular/router'
import {CORE_DIRECTIVES, NgIf, NgFor} from '@angular/common'
import {AllService} from './all.service'
import {API} from "./models/api"

@Component({
  styles: [
    ``
  ],
  template: `
  <div></div>
    `,
  directives: [ROUTER_DIRECTIVES,NgIf]
})
export class TopComponent {
  constructor(private _router: Router, private _service: AllService) {
  }

  getScreenSizeWidth(): any {
      let screen_size_width : number = $(window).width(); 
      return screen_size_width;
  }

  ngAfterViewInit() {
  }

}
