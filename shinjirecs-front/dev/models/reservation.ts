import {API, ReservationI} from './api'
import {DomSanitizationService, SafeResourceUrl} from '@angular/platform-browser'

export class Reservation implements ReservationI {
  id: number = null;
  title: string = "";
  
  api: API = null;

  static fromObject(data: ReservationI, api: API): Reservation {
    if(!data){ return null; }
    let ins = new Reservation(api);
    ins.id = data.id;
    ins.title = data.title;
    return ins;
  }

  constructor(api: API){ this.api = api; }

  public save(): boolean {
    return false;
  }
}
