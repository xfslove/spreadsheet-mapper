package excel.engine.model.meta;

import excel.engine.model.core.Sheet;

import java.io.Serializable;
import java.util.List;

/**
 * <pre>
 * sheet meta include:
 * list of field metas (see {@link FieldMeta})
 * data start at which row ({@link #getDataStartRowIndex()})
 * sheet index ({@link Sheet#getIndex()})
 * sheet name ({@link Sheet#getName()})
 * </pre>
 * Created by hanwen on 2016/12/27.
 */
public interface SheetMeta extends Serializable {

  /**
   * @return sheet name
   */
  String getName();

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
   * @return field metas sort by {@link FieldMeta#getColumnIndex()}
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
