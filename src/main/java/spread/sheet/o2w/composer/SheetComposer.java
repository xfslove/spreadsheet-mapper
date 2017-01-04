package spread.sheet.o2w.composer;

import spread.sheet.model.core.Sheet;
import spread.sheet.model.core.SheetList;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.o2w.extractor.FieldValueExtractor;

/**
 * sheet composer, generated all cell type is string (include number, date ...).
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface SheetComposer {

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
  SheetComposer fieldValueExtractor(FieldValueExtractor... fieldValueExtractors);

  /**
   * @param sheetMeta sheet meta
   */
  SheetComposer sheetMeta(SheetMeta sheetMeta);

  /**
   * @param data list of data
   */
  SheetComposer data(SheetList<Object> data);

  /**
   * @return composed sheet
   */
  Sheet compose();
}
