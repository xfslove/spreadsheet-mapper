package excel.engine.model.ext;

import excel.engine.exception.ExcelTemplateException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by hanwen on 2016/12/27.
 */
public class SheetTemplateBean implements SheetTemplate {

  private int sheetIndex;

  private int dataStartRowIndex;

  private Map<Integer, HeaderMeta> rowIndex2headerMeta = new HashMap<>();

  public SheetTemplateBean(int dataStartRowIndex, HeaderMeta... headerMetas) {
    this(1, dataStartRowIndex, headerMetas);
  }

  /**
   * <pre>
   * header meta unique with row index in one sheet (one to one),
   * if you add meta with same row index ({@link HeaderMeta#getRowIndex()}),
   * after add will override before add
   * </pre>
   *
   * @param sheetIndex        which sheet
   * @param dataStartRowIndex data start at row index
   * @param headerMetas       header metas
   */
  public SheetTemplateBean(int sheetIndex, int dataStartRowIndex, HeaderMeta... headerMetas) {
    this.sheetIndex = sheetIndex;
    this.dataStartRowIndex = dataStartRowIndex;

    if (headerMetas == null) {
      return;
    }

    for (HeaderMeta meta : headerMetas) {
      if (meta.getRowIndex() >= dataStartRowIndex) {
        throw new ExcelTemplateException("data row[" + dataStartRowIndex + "] must after " + meta.getName() + " row[" + meta.getRowIndex() + "]");
      }

      rowIndex2headerMeta.put(meta.getRowIndex(), meta);
    }
  }

  /**
   * <pre>
   * first  row : titles
   * ====================
   * second row : fields
   * ====================
   * third  row : prompts
   * ====================
   * data   row
   * ...
   * </pre>
   *
   * @param sheetIndex which sheet use default template
   * @return template
   */
  public static SheetTemplateBean DEFAULT(int sheetIndex) {
    return new SheetTemplateBean(sheetIndex, 4, HeaderMetaBean.DEFAULT_TITLE(), HeaderMetaBean.DEFAULT_FIELD(), HeaderMetaBean.DEFAULT_PROMPT());
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public int getDataStartRowIndex() {
    return dataStartRowIndex;
  }

  @Override
  public List<HeaderMeta> getHeaderMetas() {
    return new ArrayList<>(rowIndex2headerMeta.values());
  }

  @Override
  public HeaderMeta getFieldHeaderMeta() {
    for (HeaderMeta headerMeta : rowIndex2headerMeta.values()) {
      if (HeaderMeta.META_FIELD.equals(headerMeta.getName())) {
        return headerMeta;
      }
    }
    return null;
  }
}
