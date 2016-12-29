package java.excel.engine.model.ext;

import java.io.Serializable;
import java.util.List;

/**
 * <pre>
 * sheet template include which row is headers (see {@link HeaderMeta}) and data start at which row ({@link #getDataStartRowIndex()})
 *
 * default sheet template: {@link SheetTemplateBean#DEFAULT(int)}
 * </pre>
 * Created by hanwen on 2016/12/27.
 */
public interface SheetTemplate extends Serializable {

  /**
   * which sheet template
   *
   * @return 1-based
   */
  int getSheetIndex();

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
   * @return template header metas
   */
  List<HeaderMeta> getHeaderMetas();

  /**
   * @return the field meta of this template
   */
  HeaderMeta getFieldHeaderMeta();
}
