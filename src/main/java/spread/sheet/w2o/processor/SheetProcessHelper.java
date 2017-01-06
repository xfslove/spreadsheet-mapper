package spread.sheet.w2o.processor;

import spread.sheet.model.core.Sheet;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.w2o.processor.listener.CellProcessListener;
import spread.sheet.w2o.processor.listener.RowProcessListener;
import spread.sheet.w2o.processor.listener.SheetProcessListener;
import spread.sheet.w2o.setter.FieldValueSetter;

import java.util.List;

/**
 * sheet processor
 * <p>
 * Created by hanwen on 2016/12/28.
 */
public interface SheetProcessHelper<T> {

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
  @SuppressWarnings("unchecked")
  SheetProcessHelper<T> fieldValueSetter(FieldValueSetter<T>... fieldValueSetters);

  /**
   * @param objectFactory object factory
   * @see ObjectFactory
   */
  SheetProcessHelper<T> objectFactory(ObjectFactory<T> objectFactory);

  /**
   * @param sheetProcessListener sheet listener
   * @see SheetProcessListener
   */
  SheetProcessHelper<T> sheetProcessorListener(SheetProcessListener<T> sheetProcessListener);

  /**
   * @param rowProcessListener row listener
   * @see RowProcessListener
   */
  SheetProcessHelper<T> rowProcessorListener(RowProcessListener<T> rowProcessListener);

  /**
   * @param cellProcessListener cell listener
   * @see CellProcessListener
   */
  SheetProcessHelper<T> cellProcessorListener(CellProcessListener<T> cellProcessListener);

  /**
   * @param sheet sheet
   */
  SheetProcessHelper<T> sheet(Sheet sheet);

  /**
   * @param sheetMeta sheet meta
   */
  SheetProcessHelper<T> sheetMeta(SheetMeta sheetMeta);

  /**
   * @return list of data
   */
  List<T> process();
}
