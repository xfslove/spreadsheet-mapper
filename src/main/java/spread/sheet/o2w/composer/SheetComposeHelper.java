package spread.sheet.o2w.composer;

import spread.sheet.model.core.Sheet;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.o2w.extractor.FieldValueExtractor;

import java.util.List;

/**
 * sheet composer, generated all cell type is string (include number, date etc.).
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface SheetComposeHelper<T> {

  /**
   * <pre>
   * {@link FieldValueExtractor} unique with {@link FieldValueExtractor#getMatchField()} in one sheet (one to one),
   * if you add {@link FieldValueExtractor} with same {@link FieldValueExtractor#getMatchField()},
   * after add will override before add
   * </pre>
   *
   * @param fieldValueExtractors field value extractor
   * @see FieldValueExtractor
   */
  @SuppressWarnings("unchecked")
  SheetComposeHelper<T> fieldValueExtractor(FieldValueExtractor<T>... fieldValueExtractors);

  /**
   * @param sheetMeta sheet meta
   */
  SheetComposeHelper<T> sheetMeta(SheetMeta sheetMeta);

  /**
   * @param data list of data
   */
  SheetComposeHelper<T> data(List<T> data);

  /**
   * @return composed sheet
   */
  Sheet compose();
}
