import {Http} from '@angular/http'

export class Base<T> {
  public _GET(url: string): Object {
    return null;
  }
  public _POST(url: string): Object {
    return null;
  }
  public _PATCH(url: string): Object {
    return null;
  }  
  public _DELETE(url: string): Object {
    return null;
  }
  public _HEAD(url: string): Object {
    return null;
  }
  private _COMMON(url: string, method: string): Object{
    return null;
  }
}
