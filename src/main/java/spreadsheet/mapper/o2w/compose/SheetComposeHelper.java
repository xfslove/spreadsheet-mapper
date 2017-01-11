package spreadsheet.mapper.o2w.compose;

import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.o2w.compose.converter.FieldValueConverter;

import java.util.List;

/**
 * sheet composer, generated all cell type is string (include number, date etc.).
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface SheetComposeHelper<T> {

  /**
   * <pre>
   * {@link FieldValueConverter} unique with {@link FieldValueConverter#getMatchField()} in one sheet (one to one),
   * if you add {@link FieldValueConverter} with same {@link FieldValueConverter#getMatchField()},
   * after add will override before add
   * </pre>
   *
   * @param fieldValueExtractors {@link FieldValueConverter}
   */
  @SuppressWarnings("unchecked")
  SheetComposeHelper<T> fieldValueConverter(FieldValueConverter<T>... fieldValueExtractors);

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
