package spreadsheet.mapper.o2w.compose;

import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.o2w.compose.converter.FieldConverter;

import java.util.List;

/**
 * sheet compose helper, generated all cell type is string (include number, date etc.).
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface SheetComposeHelper<T> {

  /**
   * <pre>
   * {@link FieldConverter} unique with {@link FieldConverter#getMatchField()} in one sheet (one to one),
   * if you add {@link FieldConverter} with same {@link FieldConverter#getMatchField()},
   * after add will override before add
   * </pre>
   *
   * @param fieldValueExtractors {@link FieldConverter}
   * @return {@link SheetComposeHelper}
   */
  @SuppressWarnings("unchecked")
  SheetComposeHelper<T> fieldConverters(FieldConverter<T>... fieldValueExtractors);

  /**
   * @param sheetMeta {@link SheetMeta}
   * @return {@link SheetComposeHelper}
   */
  SheetComposeHelper<T> sheetMeta(SheetMeta sheetMeta);

  /**
   * @param data list of data
   * @return {@link SheetComposeHelper}
   */
  SheetComposeHelper<T> data(List<T> data);

  /**
   * @return {@link Sheet}
   */
  Sheet compose();
}
