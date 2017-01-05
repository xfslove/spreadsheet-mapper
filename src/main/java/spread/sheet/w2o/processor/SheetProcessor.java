package spread.sheet.w2o.processor;

import spread.sheet.model.core.Sheet;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.w2o.processor.listener.CellProcessorListener;
import spread.sheet.w2o.processor.listener.RowProcessorListener;
import spread.sheet.w2o.processor.listener.SheetProcessorListener;
import spread.sheet.w2o.setter.FieldValueSetter;

import java.util.List;

/**
 * sheet processor
 * <p>
 * Created by hanwen on 2016/12/28.
 */
public interface SheetProcessor {

  /**
   * <pre>
   * {@link FieldValueSetter} unique with {@link FieldValueSetter#getMatchField()} in one sheet (one to one),
   * if you add {@link FieldValueSetter} same {@link FieldValueSetter#getMatchField()},
   * after add will override before add
   * </pre>
   *
   * @param fieldValueSetters field value setter
   * @see FieldValueSetter
   */
  SheetProcessor fieldValueSetter(FieldValueSetter... fieldValueSetters);

  /**
   * @param objectFactory object factory
   * @see ObjectFactory
   */
  SheetProcessor objectFactory(ObjectFactory objectFactory);

  /**
   * @param sheetProcessorListener sheet listener
   * @see SheetProcessorListener
   */
  SheetProcessor sheetProcessorListener(SheetProcessorListener sheetProcessorListener);

  /**
   * @param rowProcessorListener row listener
   * @see RowProcessorListener
   */
  SheetProcessor rowProcessorListener(RowProcessorListener rowProcessorListener);

  /**
   * @param cellProcessorListener cell listener
   * @see CellProcessorListener
   */
  SheetProcessor cellProcessorListener(CellProcessorListener cellProcessorListener);

  /**
   * @param sheet sheet
   */
  SheetProcessor sheet(Sheet sheet);

  /**
   * @param sheetMeta sheet meta
   */
  SheetProcessor sheetMeta(SheetMeta sheetMeta);

  /**
   * @return list of data
   */
  List<Object> process();
}
