package excel.engine.o2w.composer;

import excel.engine.model.core.Sheet;
import excel.engine.model.meta.SheetMeta;
import excel.engine.o2w.extractor.FieldValueExtractor;

import java.util.List;

/**
 * excel composer, generated all cell type is string (include number, date ...).
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface SheetComposer {

  /**
   * <pre>
   * field value extractor unique with object field in one sheet (one to one),
   * if you add extractor with same match field({@link FieldValueExtractor#getMatchField()}),
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
  SheetComposer data(List<Object> data);

  Sheet compose();
}
