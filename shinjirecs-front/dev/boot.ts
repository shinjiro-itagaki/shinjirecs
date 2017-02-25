///<reference path="../typings/main.d.ts"/>
import {bootstrap} from '@angular/platform-browser-dynamic'
import {provideRouter} from '@angular/router'
import {AppComponent} from './app.component'
import {AllService} from './all.service'
import {enableProdMode} from '@angular/core';

import {TopComponent}   from './top.component'
import {BROWSER_SANITIZATION_PROVIDERS, DomSanitizationService, SafeResourceUrl} from '@angular/platform-browser'

const routings = [
  { path: '',                 component: TopComponent             , data: {'hide_header': true}},
  { path: '**',               component: TopComponent             , data: {'hide_header': true}},
];
enableProdMode();
bootstrap(AppComponent, [
  provideRouter(routings, {enableTracing: true}), AllService, DomSanitizationService, BROWSER_SANITIZATION_PROVIDERS // serviceをsingletonで扱いたい場合はここで指定する
]);
