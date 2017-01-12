package spreadsheet.mapper.w2o.process;

import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.w2o.process.factory.ObjectFactory;
import spreadsheet.mapper.w2o.process.listener.CellProcessListener;
import spreadsheet.mapper.w2o.process.listener.RowProcessListener;
import spreadsheet.mapper.w2o.process.listener.SheetProcessListener;
import spreadsheet.mapper.w2o.process.setter.FieldValueSetter;

import java.util.List;

/**
 * sheet process helper
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
   * @param fieldValueSetters {@link FieldValueSetter}
   * @return {@link SheetProcessHelper}
   * @see FieldValueSetter
   */
  @SuppressWarnings("unchecked")
  SheetProcessHelper<T> fieldValueSetters(FieldValueSetter<T>... fieldValueSetters);

  /**
   * @param objectFactory {@link ObjectFactory}
   * @return {@link SheetProcessHelper}
   */
  SheetProcessHelper<T> objectFactory(ObjectFactory<T> objectFactory);

  /**
   * @param sheetProcessListener {@link SheetProcessListener}
   * @return {@link SheetProcessHelper}
   */
  SheetProcessHelper<T> sheetProcessorListener(SheetProcessListener<T> sheetProcessListener);

  /**
   * @param rowProcessListener {@link RowProcessListener}
   * @return {@link SheetProcessHelper}
   */
  SheetProcessHelper<T> rowProcessorListener(RowProcessListener<T> rowProcessListener);

  /**
   * @param cellProcessListener {@link CellProcessListener}
   * @return {@link SheetProcessHelper}
   */
  SheetProcessHelper<T> cellProcessorListener(CellProcessListener<T> cellProcessListener);

  /**
   * @param sheet {@link Sheet}
   * @return {@link SheetProcessHelper}
   */
  SheetProcessHelper<T> sheet(Sheet sheet);

  /**
   * @param sheetMeta {@link SheetMeta}
   * @return {@link SheetProcessHelper}
   */
  SheetProcessHelper<T> sheetMeta(SheetMeta sheetMeta);

  /**
   * @return list of data
   */
  List<T> process();
}
