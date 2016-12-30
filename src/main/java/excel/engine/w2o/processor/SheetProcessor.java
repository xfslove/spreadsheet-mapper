package excel.engine.w2o.processor;

import excel.engine.model.core.Sheet;
import excel.engine.model.meta.SheetMeta;
import excel.engine.w2o.setter.FieldValueSetter;

import java.util.List;

/**
 * sheet processor
 * <p>
 * Created by hanwen on 2016/12/28.
 */
public interface SheetProcessor {

  /**
   * <pre>
   * field value setter unique with object field in one sheet (one to one),
   * if you add setter with same match field({@link FieldValueSetter#getMatchField()}),
   * after add will override before add
   * </pre>
   *
   * @param setters field value setter
   * @see FieldValueSetter
   */
  SheetProcessor fieldValueSetter(FieldValueSetter... setters);

  /**
   * @param objectFactory object factory
   * @see ObjectFactory
   */
  SheetProcessor objectFactory(ObjectFactory objectFactory);

  /**
   * @param sheetProcessorListener listener
   * @see SheetProcessorListener
   */
  SheetProcessor sheetProcessorListener(SheetProcessorListener sheetProcessorListener);

  /**
   * @param sheet sheet
   */
  SheetProcessor sheet(Sheet sheet);

  /**
   * @param sheetMeta sheet meta
   */
  SheetProcessor sheetMeta(SheetMeta sheetMeta);

  /**
   * @return list of sheets data
   */
  List<Object> process();
}
