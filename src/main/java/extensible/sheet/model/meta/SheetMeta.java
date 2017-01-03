package extensible.sheet.model.meta;

import extensible.sheet.model.core.Sheet;

import java.io.Serializable;
import java.util.List;

/**
 * <pre>
 * sheet meta include:
 * list of field meta (see {@link FieldMeta})
 * data start at which row ({@link #getDataStartRowIndex()})
 * sheet name ({@link Sheet#getName()})
 * </pre>
 * Created by hanwen on 2016/12/27.
 */
public interface SheetMeta extends Serializable {

  /**
   * @return sheet index
   */
  int getSheetIndex();

  /**
   * @return sheet name
   */
  String getSheetName();

  /**
   * <pre>
   * data row start at index
   * data start at which row (must be after header rows if has)
   * </pre>
   *
   * @return 1-based
   */
  int getDataStartRowIndex();

  /**
   * @return list of field meta
   */
  List<FieldMeta> getFieldMetas();

  /**
   * @param field field
   * @return field meta name of field
   */
  FieldMeta getFieldMeta(String field);

  /**
   * add field meta
   *
   * @param fieldMeta field meta
   */
  void addFieldMeta(FieldMeta fieldMeta);
}
