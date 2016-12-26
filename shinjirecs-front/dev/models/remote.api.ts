import {Injectable} from '@angular/core'
import {API,ReservationI} from "./api"
import {Http,RequestOptionsArgs,Headers,Response} from '@angular/http';
import 'rxjs/add/operator/map';
import {Observable} from 'rxjs/Rx';

/// <reference path="./../../config/config.d.ts" />

@Injectable()
export class RemoteAPI implements API {

  public onNetworkError : () => void = null;
  public onTimeoutError : () => void = null;
  public onSessionTimeout : () => void = null;
  
  public static parseToDate(str: string) : Date {
    return (str) ? new Date(str.replace(/T/g,' ').replace(/-/g,'/')) : null;
  }
  
  constructor () {}

  public static toBoolean(obj:any, success:boolean): boolean {
    if(obj === true || obj === false){
      return obj;
    }else{
      return success;
    }
  }

  public static toString(obj:any, success:boolean): string {
    if(success){
      return String(obj);
    }else{
      return "";
    }
  }

  public static toReservationI(obj:any, success:boolean): ReservationI {
    if(!obj || !success) { return null; }
    return {
      id : obj.id,
      title : obj.title
    };
  }

  public static toReservationIs(objs:any, success:boolean): ReservationI[] {
    if(!objs || !success) { return []; }
    return objs.map(function(obj: any){
      return RemoteAPI.toReservationI(obj,success);
    });    
  }
  
  private access(url: string, method: string, body: string) : {body: string, ok: boolean}{
    let xhr = new XMLHttpRequest();
    if(Config.logging){ console.log(url);}
    xhr.withCredentials = false; // cookieによるセッション情報の送信をやめたのでfalse
    xhr.ontimeout = () => {
      if(Config.logging){ console.error("The " + method + " request for " + url + " timed out.");}
      if(this.onTimeoutError){
	this.onTimeoutError();
      }      
    };
    let res_ok:boolean = false;
    let res_body:string = ""
    xhr.onload = () => {
      res_body = xhr.responseText;
      if(Config.logging){ console.log(res_body);}
      if(xhr.status > 199 && xhr.status < 300){
	res_ok = true;
      }else{
	if(xhr.status == 401){
	  if(this.onSessionTimeout){
	    this.onSessionTimeout();
	  }
	}
      }
    }
    xhr.onerror = () => {
      if(xhr.status == 0){ //ネットワークエラー
	if(this.onNetworkError){
	  this.onNetworkError();
	}
      }
    }
    xhr.open(method, url, false);
    xhr.setRequestHeader("Content-Type","application/json");
    // let sid = this.sessionId();
    // if(sid){
      // 値がnullだと"null"という文字列が書き込まれてしまう
      //xhr.setRequestHeader("Session-Id", sid); // このヘッダ項目は.htaccessのAccess-Control-Allow-Headersに許可を追加している
    //}
    xhr.send(body);
    return {body: res_body, ok: res_ok};
  }
  
  private _COMMON<T>(res: {body: string, ok: boolean}, f: (v: any, success: boolean) => T) : T {  
    if(res.ok){
      var result : any = null;
      try{
	let json = JSON.parse(res.body);
	result = json.result;
	/*
	if(json.session_id !== undefined){
          this.saveSessionId(json.session_id);
	}
	*/
      }catch(e){
      }
      return f(result, true);
    }else{
      return f(null, false);
    }
  }

  private mkUrl(path: string): string {
    return Config.api_server.url + path + ".json";
  }
  
  private _GET<T>(path: string, params: string, f: (res: any, success: boolean) => T) : T {
    var url = this.mkUrl(path);
    if(params){
      url += "?" + params
    }
    return this._COMMON(this.access(url, "GET", null), f);
  }

  private _POST<T>(path: string, params: any, f: (res: any ,success: boolean) => T) : T {
    return this._COMMON(this.access(this.mkUrl(path),"POST", JSON.stringify(params)), f);
  }

  private _PATCH<T>(path: string, params: any, f: (res: any ,success: boolean) => T) : T {        
    return this._COMMON(this.access(this.mkUrl(path),"PATCH", JSON.stringify(params)), f);
  }

  private _DELETE<T>(path: string, params: any, f: (res: any ,success: boolean) => T) : T {
    return this._COMMON(this.access(this.mkUrl(path),"DELETE",JSON.stringify(params)), f);
  }

  // private _HEAD(path: string): any {
  //   let url = Config.api_server.url + path;
  //   return this._http.head(url).map(res => res.json()).result;
  // }

  get_reservations(): ReservationI[] {
    return this._GET<ReservationI[]>(`/reservations`, null, RemoteAPI.toReservationIs)
  }  
}
