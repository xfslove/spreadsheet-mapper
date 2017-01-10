package spreadsheet.mapper.o2w.composer;

import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.o2w.extractor.FieldValueExtractor;

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
   * @param fieldValueExtractors {@link FieldValueExtractor}
   */
  @SuppressWarnings("unchecked")
  SheetComposeHelper<T> fieldValueExtractor(FieldValueExtractor<T>... fieldValueExtractors);

  /**
   * @param sheetMeta {@link SheetMeta}
   */
  SheetComposeHelper<T> sheetMeta(SheetMeta sheetMeta);

  /**
   * @param data list of data
   */
  SheetComposeHelper<T> data(List<T> data);

  /**
   * @return {@link Sheet}
   */
  Sheet compose();
}
