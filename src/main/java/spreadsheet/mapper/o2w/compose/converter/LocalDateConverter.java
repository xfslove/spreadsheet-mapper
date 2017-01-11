package spreadsheet.mapper.o2w.compose.converter;

import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.joda.time.LocalDate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.o2w.compose.WorkbookComposeException;
import spreadsheet.mapper.utils.FieldUtils;

/**
 * local date text value with supplied pattern converter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateConverter<T> extends FieldValueConverterAdapter<T> {

  private static final Logger LOGGER = LoggerFactory.getLogger(LocalDateConverter.class);

  private String pattern;

  public LocalDateConverter(String pattern, String matchField) {
    super(matchField);
    this.pattern = pattern;
  }

  @Override
  public String getStringValue(T object, FieldMeta fieldMeta) {

    try {
      Object value = PropertyUtils.getProperty(object, FieldUtils.detectRealFieldName(fieldMeta));

      if (!(value instanceof LocalDate)) {
        return null;
      }

      return ((LocalDate) value).toString(pattern);

    } catch (NestedNullException e) {
      LOGGER.debug("{} is null", fieldMeta.getName());
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookComposeException(e);
    }
  }
}
